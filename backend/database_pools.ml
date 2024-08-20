open CCFun.Infix

exception Exception of string

module type Sig = Database_pools_sig.Sig

let src = Logs.Src.create "guardian.pools"
let find_pool_name = CCList.assoc_opt ~eq:CCString.equal "pool"

module LogTag = struct
  let add_label : string Logs.Tag.def =
    Logs.Tag.def "database_label" ~doc:"Database Label" CCString.pp
  ;;

  let create database = Logs.Tag.(empty |> add add_label database)

  let ctx_opt ?ctx () =
    let open CCOption.Infix in
    ctx >>= find_pool_name >|= fun db -> Logs.Tag.(empty |> add add_label db)
  ;;
end

let raise_caqti_error =
  let open Caqti_error in
  function
  | Ok resp -> resp
  | Error `Unsupported -> raise (Exception "Caqti error: Unsupported")
  | Error (#t as err) -> raise (Exn err)
;;

module type ConfigSig = sig
  val database : string * string
  val database_pool_size : int
  val expected_databases : int
end

module DefaultConfig : ConfigSig = struct
  let database = "main", "mariadb://root@database:3306/test"
  let database_pool_size = 5
  let expected_databases = 1
end

module Make (Config : ConfigSig) = struct
  module Config = Config

  type connection =
    | Close
    | Open of (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t
    | Fail of Caqti_error.load

  module Pool = struct
    type t =
      { database_label : string
      ; database_url : string
      ; required : bool
      ; connection : connection [@opaque]
      ; n_retries : int
      }
    [@@deriving fields]

    let create ?(required = false) database_label database_url =
      { database_label
      ; database_url
      ; required
      ; connection = Close
      ; n_retries = 0
      }
    ;;

    let reset_retry pool = { pool with n_retries = 0 }
    let increment_retry pool = { pool with n_retries = pool.n_retries + 1 }

    let connect_pool =
      Uri.of_string
      %> Caqti_lwt_unix.connect_pool
           ~pool_config:
             (Caqti_pool_config.create ~max_size:Config.database_pool_size ())
    ;;

    let connect
      ?(retries = 2)
      ({ database_label; database_url; required; _ } as pool)
      =
      let tags = database_label |> LogTag.create in
      CCResult.retry retries (fun () -> database_url |> connect_pool)
      |> (function
            | Error [] -> raise (Exception "Failed to connect: empty error")
            | Error (err :: _) when required -> raise_caqti_error (Error err)
            | Error (err :: _ as errors) ->
              Logs.warn ~src (fun m ->
                m
                  ~tags
                  "Failed to connect: %s (%s)"
                  database_label
                  ([%show: Caqti_error.t list] errors));
              Fail err
            | Ok con -> Open con)
      |> fun connection -> { pool with connection }
    ;;
  end

  module Cache = struct
    module Hashtbl = CCHashtbl.Make (CCString)

    let pools : Pool.t Hashtbl.t =
      Hashtbl.create (max 1 Config.expected_databases)
    ;;

    let add = Hashtbl.add pools
    let remove = Hashtbl.remove pools
    let find_opt = Hashtbl.find_opt pools
    let replace pool = Hashtbl.replace pools (Pool.database_label pool) pool
  end

  let print_pool_usage ?tags =
    Pool.connection
    %> function
    | Open pool ->
      let n_connections = Caqti_lwt_unix.Pool.size pool in
      Logs.debug ~src (fun m ->
        m ?tags "Pool usage: %i/%i" n_connections Config.database_pool_size)
    | Close | Fail _ ->
      Logs.debug ~src (fun m -> m ?tags "Pool usage: No connection found")
  ;;

  let drain_opt =
    Pool.connection
    %> function
    | Open pool -> Caqti_lwt_unix.Pool.drain pool
    | Close | Fail _ -> Lwt.return_unit
  ;;

  let rec fetch_pool ?(ctx = []) ?(retries = 2) () =
    match ctx |> find_pool_name |> CCFun.flip CCOption.bind Cache.find_opt with
    | Some pool ->
      (match Pool.connection pool with
       | Fail err when pool.Pool.n_retries >= retries ->
         raise_caqti_error (Error err)
       | Fail _ ->
         let () = Pool.connect pool |> Pool.increment_retry |> Cache.replace in
         fetch_pool ~ctx ~retries ()
       | Close ->
         let () = Pool.connect pool |> Cache.replace in
         fetch_pool ~ctx ~retries ()
       | Open connection when pool.Pool.n_retries > 0 ->
         let () = Pool.reset_retry pool |> Cache.replace in
         print_pool_usage ?tags:(LogTag.ctx_opt ~ctx ()) pool;
         connection
       | Open connection ->
         print_pool_usage ?tags:(LogTag.ctx_opt ~ctx ()) pool;
         connection)
    | None ->
      raise
        (Exception
           (Format.asprintf
              "Unknown Pool: Please 'add_pool' first! (%s)"
              CCOption.(find_pool_name ctx |> value ~default:"-")))
  ;;

  let add_pool ?required database_label database_url =
    match Cache.find_opt database_label with
    | Some _ ->
      let msg =
        [%string "Failed to add pool: Pool already exists %{database_label}"]
      in
      Logs.err ~src (fun m ->
        m ~tags:(database_label |> LogTag.create) "%s" msg);
      failwith msg
    | None ->
      Pool.create ?required database_label database_url
      |> Cache.add database_label
  ;;

  let drop_pool name =
    match Cache.find_opt name with
    | None ->
      let msg =
        [%string "Failed to drop pool: connection to '%{name}' doesn't exist"]
      in
      Logs.info ~src (fun m -> m ~tags:(LogTag.create name) "%s" msg);
      Lwt.return_unit
    | Some pool ->
      let%lwt () = drain_opt pool in
      Cache.remove name |> Lwt.return
  ;;

  let initialize ?(additinal_pools : (string * string) list = []) () : unit =
    Config.database :: additinal_pools
    |> CCList.filter (fst %> Cache.find_opt %> CCOption.is_none)
    |> CCList.iter (CCFun.uncurry (Pool.create ~required:true) %> Cache.replace)
  ;;

  let connect =
    Cache.find_opt
    %> function
    | Some pool ->
      let rec connect pool =
        match pool.Pool.connection with
        | Fail err -> Error (Caqti_error.show err)
        | Close -> Pool.connect pool |> connect
        | Open _ -> Ok ()
      in
      connect pool
    | None -> Error "Database not foun"
  ;;

  let disconnect =
    Cache.find_opt
    %> function
    | Some pool ->
      let%lwt () = drain_opt pool in
      Cache.replace { pool with Pool.connection = Close } |> Lwt.return
    | None -> Lwt.return_unit
  ;;

  let map_fetched ?ctx (fcn : 'a -> ('b, 'e) Lwt_result.t) =
    fetch_pool ?ctx () |> fcn
  ;;

  let query ?ctx f =
    let open Lwt.Infix in
    Caqti_lwt_unix.Pool.use (fun connection -> f connection)
    |> map_fetched ?ctx
    >|= raise_caqti_error
  ;;

  let find_opt ?ctx request input =
    query ?ctx (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.find_opt request input)
  ;;

  let find ?ctx request input =
    query ?ctx (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.find request input)
  ;;

  let collect ?ctx request input =
    query ?ctx (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.collect_list request input)
  ;;

  let exec ?ctx request input =
    query ?ctx (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec request input)
  ;;

  let populate ?ctx table columns request input =
    query ?ctx (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.populate
        ~table
        ~columns
        request
        (Caqti_lwt.Stream.of_list input)
      |> Lwt.map Caqti_error.uncongested)
  ;;

  let exec_each connection =
    let open CCFun.Infix in
    Lwt_list.map_s (fun request -> request connection)
    %> Lwt.map CCResult.flatten_l
    %> Lwt_result.map (fun (_ : unit list) -> ())
  ;;

  let rollback connection error =
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    let%lwt () =
      Connection.rollback ()
      |> Lwt_result.map
           (CCFun.tap (fun _ ->
              Logs.debug (fun m -> m "Successfully rolled back transaction")))
      |> Lwt.map raise_caqti_error
    in
    Lwt.fail error
  ;;

  let transaction
    ?ctx
    ?(setup : (Caqti_lwt.connection -> (unit, Caqti_error.t) Lwt_result.t) list =
      [])
    ?(cleanup :
        (Caqti_lwt.connection -> (unit, Caqti_error.t) Lwt_result.t) list =
      [])
    (f : Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
    : 'a Lwt.t
    =
    let open Lwt_result.Syntax in
    Caqti_lwt_unix.Pool.use (fun connection ->
      let (module Connection : Caqti_lwt.CONNECTION) = connection in
      let* () = Connection.start () in
      Lwt.catch
        (fun () ->
          let* () = exec_each connection setup in
          let* result = f connection in
          let* () = exec_each connection cleanup in
          match%lwt Connection.commit () with
          | Ok () -> Lwt.return_ok result
          | Error error -> Lwt.return_error error)
        (rollback connection))
    |> map_fetched ?ctx
    |> Lwt.map raise_caqti_error
  ;;

  let transaction_iter ?ctx queries =
    let open Lwt_result.Syntax in
    Caqti_lwt_unix.Pool.use (fun connection ->
      let (module Connection : Caqti_lwt.CONNECTION) = connection in
      let* () = Connection.start () in
      Lwt.catch
        (fun () ->
          let* () = exec_each connection queries in
          Connection.commit ())
        (rollback connection))
    |> map_fetched ?ctx
    |> Lwt.map raise_caqti_error
  ;;
end
