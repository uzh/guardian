exception Exception of string

type connection_type =
  | SinglePool of string
  | MultiPools of (string * string) list

let with_log ?(log_level = Logs.Error) ?(msg_prefix = "Error") err =
  let msg = Caqti_error.show err in
  Logs.msg log_level (fun m -> m "%s: %s" msg_prefix msg);
  msg
;;

let get_or_raise ?log_level ?msg_prefix () = function
  | Ok result -> result
  | Error error -> raise @@ Exception (with_log ?log_level ?msg_prefix error)
;;

let map_or_raise ?log_level ?msg_prefix fcn result =
  result |> CCResult.map fcn |> get_or_raise ?log_level ?msg_prefix ()
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
    : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t option ref
    =
    ref None
  ;;

  let pools
    : (string, (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t) Hashtbl.t
    =
    let spare_for_pools = 5 in
    Hashtbl.create
      (match Config.database with
       | SinglePool _ -> 1
       | MultiPools pools -> CCList.length pools + spare_for_pools)
  ;;

  let print_pool_usage pool =
    let n_connections = Caqti_lwt.Pool.size pool in
    let max_connections = Config.database_pool_size in
    Logs.debug (fun m -> m "Pool usage: %i/%i" n_connections max_connections)
  ;;

  let connect_or_failwith
    ?(pool_size = Config.database_pool_size)
    ok_fun
    database_url
    =
    database_url
    |> Uri.of_string
    |> Caqti_lwt.connect_pool ~max_size:pool_size
    |> map_or_raise ~msg_prefix:"Failed to connect to DB pool" ok_fun
  ;;

  let add_pool ?pool_size name database_url =
    match Config.database with
    | SinglePool _ ->
      raise @@ Exception "SinglePool is selected: Switch to 'MultiPools' first"
    | MultiPools _ when CCOption.is_some (Hashtbl.find_opt pools name) ->
      let msg =
        Format.asprintf
          "Failed to create pool: Connection pool with name '%s' exists already"
          name
      in
      Logs.err (fun m -> m "%s" msg);
      raise @@ Exception msg
    | MultiPools _ ->
      database_url |> connect_or_failwith ?pool_size (Hashtbl.add pools name)
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
      CCList.assoc_opt ~eq:CCString.equal "pool" ctx
      >>= Hashtbl.find_opt pools
      |> (function
      | Some pool -> pool
      | None -> raise @@ Exception "Unknown Pool: Please 'add_pool' first!")
  ;;

  let transaction ?ctx f =
    let open Lwt.Infix in
    let pool = fetch_pool ?ctx () in
    print_pool_usage pool;
    Caqti_lwt.Pool.use
      (fun connection ->
        Logs.debug (fun m -> m "Fetched connection from pool");
        let (module Connection : Caqti_lwt.CONNECTION) = connection in
        let open Caqti_error in
        match%lwt Connection.start () with
        | Error msg ->
          Logs.debug (fun m -> m "Failed to start transaction: %s" (show msg));
          Lwt.return_error msg
        | Ok () ->
          Logs.debug (fun m -> m "Started transaction");
          Lwt.catch
            (fun () ->
              match%lwt Connection.commit () with
              | Ok () ->
                Logs.debug (fun m -> m "Successfully committed transaction");
                f connection |> Lwt_result.return
              | Error error ->
                Exception
                  (with_log ~msg_prefix:"Failed to commit transaction" error)
                |> Lwt.fail)
            (fun e ->
              match%lwt Connection.rollback () with
              | Ok () ->
                Logs.debug (fun m -> m "Successfully rolled back transaction");
                Lwt.fail e
              | Error error ->
                Exception
                  (with_log ~msg_prefix:"Failed to rollback transaction" error)
                |> Lwt.fail))
      pool
    >|= get_or_raise ()
  ;;

  let transaction' ?ctx f = transaction ?ctx f |> Lwt.map (get_or_raise ())

  let query ?ctx f =
    let open Lwt.Infix in
    let pool = fetch_pool ?ctx () in
    print_pool_usage pool;
    Caqti_lwt.Pool.use
      (fun connection ->
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        f connection >|= CCResult.return)
      pool
    >|= get_or_raise ()
  ;;

  let query' ?ctx f = query ?ctx f |> Lwt.map (get_or_raise ())

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
end

module type Sig = sig
  val initialize : unit -> unit

  val fetch_pool
    :  ?ctx:(string * string) list
    -> unit
    -> (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t

  val add_pool : ?pool_size:int -> string -> string -> unit

  val find_opt
    :  ?ctx:(string * string) list
    -> ('a, 'b, [< `One | `Zero ]) Caqti_request.t
    -> 'a
    -> 'b option Lwt.t

  val find
    :  ?ctx:(string * string) list
    -> ('a, 'b, [< `One ]) Caqti_request.t
    -> 'a
    -> 'b Lwt.t

  val collect
    :  ?ctx:(string * string) list
    -> ('a, 'b, [< `Many | `One | `Zero ]) Caqti_request.t
    -> 'a
    -> 'b list Lwt.t

  val exec
    :  ?ctx:(string * string) list
    -> ('a, unit, [< `Zero ]) Caqti_request.t
    -> 'a
    -> unit Lwt.t
end
