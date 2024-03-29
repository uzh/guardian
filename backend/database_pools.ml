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

type connection_type =
  | SinglePool of string
  | MultiPools of (string * string) list

let with_log ?tags ?(log_level = Logs.Error) ?(msg_prefix = "Error") err =
  let msg = Caqti_error.show err in
  Logs.msg ~src log_level (fun m -> m ?tags "%s: %s" msg_prefix msg);
  msg
;;

let get_or_raise ?ctx ?tags ?log_level ?msg_prefix () =
  let tags = CCOption.or_ ~else_:(LogTag.ctx_opt ?ctx ()) tags in
  function
  | Ok result -> result
  | Error error -> failwith (with_log ?tags ?log_level ?msg_prefix error)
;;

let map_or_raise ?ctx ?tags ?log_level ?msg_prefix fcn result =
  result
  |> CCResult.map fcn
  |> get_or_raise ?ctx ?tags ?log_level ?msg_prefix ()
;;

module type ConfigSig = sig
  val database : connection_type
  val database_pool_size : int
end

module DefaultConfig : ConfigSig = struct
  let database = SinglePool "mariadb://root@database:3306/test"
  let database_pool_size = 5
end

module Make (Config : ConfigSig) = struct
  let main_pool_ref
    : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t option ref
    =
    ref None
  ;;

  let pools
    : ( string
        , (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t )
        Hashtbl.t
    =
    let spare_for_pools = 5 in
    Hashtbl.create
      (match Config.database with
       | SinglePool _ -> 1
       | MultiPools pools -> CCList.length pools + spare_for_pools)
  ;;

  let print_pool_usage ?tags pool =
    let n_connections = Caqti_lwt_unix.Pool.size pool in
    let max_connections = Config.database_pool_size in
    Logs.debug ~src (fun m ->
      m ?tags "Pool usage: %i/%i" n_connections max_connections)
  ;;

  let connect_or_failwith
    ?(pool_size = Config.database_pool_size)
    ok_fun
    database_url
    =
    database_url
    |> Uri.of_string
    |> Caqti_lwt_unix.connect_pool
         ~pool_config:(Caqti_pool_config.create ~max_size:pool_size ())
    |> map_or_raise ~msg_prefix:"Failed to connect to DB pool" ok_fun
  ;;

  let add_pool ?pool_size name database_url =
    let tags = LogTag.create name in
    match Config.database, Hashtbl.find_opt pools name with
    | SinglePool _, _ ->
      failwith "SinglePool is selected: Switch to 'MultiPools' first"
    | MultiPools _, Some _ ->
      let msg =
        Format.asprintf
          "Failed to create pool: Connection pool with name '%s' exists already"
          name
      in
      Logs.err ~src (fun m -> m ~tags "%s" msg);
      failwith msg
    | MultiPools _, None ->
      database_url |> connect_or_failwith ?pool_size (Hashtbl.add pools name)
  ;;

  let drop_pool name =
    let tags = LogTag.create name in
    match Config.database, Hashtbl.find_opt pools name with
    | SinglePool _, _ ->
      failwith "SinglePool is selected: Switch to 'MultiPools' first"
    | MultiPools _, None ->
      let msg =
        Format.asprintf
          "Failed to drop pool: connection to '%s' doesn't exist"
          name
      in
      Logs.warn ~src (fun m -> m ~tags "%s" msg);
      Lwt.return_unit
    | MultiPools _, Some connection ->
      let () = Hashtbl.remove pools name in
      let%lwt () = Caqti_lwt_unix.Pool.drain connection in
      Lwt.return_unit
  ;;

  let initialize () =
    match Config.database with
    | SinglePool database_url when CCOption.is_none !main_pool_ref ->
      database_url
      |> connect_or_failwith (fun pool ->
        main_pool_ref := Some pool;
        ())
    | SinglePool _ -> ()
    | MultiPools pools' ->
      pools'
      |> CCList.filter (fun (name, _) ->
        CCOption.is_none (Hashtbl.find_opt pools name))
      |> CCList.iter (fun (name, url) ->
        url |> connect_or_failwith (Hashtbl.add pools name))
  ;;

  let fetch_pool ?(ctx = []) () =
    let open CCOption in
    let () = initialize () in
    match Config.database with
    | SinglePool _ ->
      !main_pool_ref |> get_exn_or "Initialization missed: run 'initialize'"
    | MultiPools _ ->
      find_pool_name ctx
      >>= Hashtbl.find_opt pools
      |> (function
       | Some pool -> pool
       | None -> failwith "Unknown Pool: Please 'add_pool' first!")
  ;;

  let transaction ?ctx f =
    let open Lwt.Infix in
    let pool = fetch_pool ?ctx () in
    print_pool_usage pool;
    Caqti_lwt_unix.Pool.use
      (fun connection ->
        Logs.debug ~src (fun m ->
          m ?tags:(LogTag.ctx_opt ?ctx ()) "Fetched connection from pool");
        let (module Connection : Caqti_lwt.CONNECTION) = connection in
        let open Caqti_error in
        match%lwt Connection.start () with
        | Error msg ->
          Logs.debug ~src (fun m ->
            m
              ?tags:(LogTag.ctx_opt ?ctx ())
              "Failed to start transaction: %s"
              (show msg));
          Lwt.return_error msg
        | Ok () ->
          Logs.debug ~src (fun m ->
            m ?tags:(LogTag.ctx_opt ?ctx ()) "Started transaction");
          Lwt.catch
            (fun () ->
              match%lwt Connection.commit () with
              | Ok () ->
                Logs.debug ~src (fun m ->
                  m
                    ?tags:(LogTag.ctx_opt ?ctx ())
                    "Successfully committed transaction");
                f connection |> Lwt_result.return
              | Error error ->
                Exception
                  (with_log
                     ?tags:(LogTag.ctx_opt ?ctx ())
                     ~msg_prefix:"Failed to commit transaction"
                     error)
                |> Lwt.fail)
            (fun e ->
              match%lwt Connection.rollback () with
              | Ok () ->
                Logs.debug ~src (fun m ->
                  m
                    ?tags:(LogTag.ctx_opt ?ctx ())
                    "Successfully rolled back transaction");
                Lwt.fail e
              | Error error ->
                Exception
                  (with_log
                     ?tags:(LogTag.ctx_opt ?ctx ())
                     ~msg_prefix:"Failed to rollback transaction"
                     error)
                |> Lwt.fail))
      pool
    >|= get_or_raise ?ctx ()
  ;;

  let transaction' ?ctx f = transaction ?ctx f |> Lwt.map (get_or_raise ?ctx ())

  let exec_with_connection
    (request : ('a, unit, [< `Zero ]) Caqti_request.t)
    (input : 'a)
    (connection : (module Caqti_lwt.CONNECTION))
    : unit Lwt.t
    =
    let open CCFun in
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    Connection.exec request input
    |> Lwt.map CCResult.(map_err Caqti_error.show %> get_or_failwith)
  ;;

  let query ?ctx f =
    let open Lwt.Infix in
    let pool = fetch_pool ?ctx () in
    print_pool_usage pool;
    Caqti_lwt_unix.Pool.use
      (fun connection -> f connection >|= CCResult.return)
      pool
    >|= get_or_raise ?ctx ()
  ;;

  let query' ?ctx f = query ?ctx f |> Lwt.map (get_or_raise ?ctx ())

  let find_opt ?ctx request input =
    query' ?ctx (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.find_opt request input)
  ;;

  let find ?ctx request input =
    query' ?ctx (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.find request input)
  ;;

  let collect ?ctx request input =
    query' ?ctx (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.collect_list request input)
  ;;

  let exec ?ctx request input =
    query' ?ctx (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec request input)
  ;;

  let populate ?ctx table columns request input =
    query' ?ctx (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.populate
        ~table
        ~columns
        request
        (Caqti_lwt.Stream.of_list input)
      |> Lwt.map Caqti_error.uncongested)
  ;;
end
