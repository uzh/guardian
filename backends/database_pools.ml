exception Exception of string

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
  val database_url : string
  val database_pool_size : int option
  val database_skip_default_pool_creation : bool option
  val database_choose_pool : string option
  val database_max_pools : int option
end

module DefaultConfig : ConfigSig = struct
  let database_url = "mariadb://root@database:3306/dev"
  let database_choose_pool = None
  let database_skip_default_pool_creation = None
  let database_pool_size = None
  let database_max_pools = None
end

module Make (CONFIG : ConfigSig) () = struct
  module Config = struct
    module Database = struct
      let url = CONFIG.database_url
      let pool_size = CCOption.get_or ~default:5 CONFIG.database_pool_size

      let skip_default_pool_creation =
        CCOption.get_or
          ~default:false
          CONFIG.database_skip_default_pool_creation
      ;;

      let choose_pool = CONFIG.database_choose_pool
      let max_pools = CCOption.get_or ~default:100 CONFIG.database_max_pools
    end
  end

  let main_pool_ref
    : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t option ref
    =
    ref None
  ;;

  let pools
    : (string, (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t) Hashtbl.t
    =
    Hashtbl.create Config.Database.max_pools
  ;;

  let print_pool_usage pool =
    let n_connections = Caqti_lwt.Pool.size pool in
    let max_connections = Config.Database.pool_size in
    Logs.debug (fun m -> m "Pool usage: %i/%i" n_connections max_connections)
  ;;

  let connect_or_failwith pool_size ok_fun database_url =
    database_url
    |> Uri.of_string
    |> Caqti_lwt.connect_pool ~max_size:pool_size
    |> map_or_raise ~msg_prefix:"Failed to connect to DB pool" ok_fun
  ;;

  let fetch_pool ?(ctx = []) () =
    let chosen_pool_name_ctx = CCList.assoc_opt ~eq:String.equal "pool" ctx in
    let chosen_pool =
      let open CCOption in
      choice [ chosen_pool_name_ctx; Config.Database.choose_pool ]
      >>= Hashtbl.find_opt pools
    in
    match chosen_pool, !main_pool_ref with
    | Some pool, _ -> pool
    | None, Some pool ->
      Logs.debug (fun m -> m "Skipping pool creation, re-using existing pool");
      pool
    | None, None ->
      if Config.Database.skip_default_pool_creation
      then
        Logs.warn (fun m ->
          m
            "database_skip_default_pool_creation config was set to true, but \
             no pool was defined for querying.");
      let pool_size = Config.Database.pool_size in
      Logs.info (fun m -> m "Create pool with size %i" pool_size);
      Config.Database.url
      |> connect_or_failwith pool_size (fun pool ->
           main_pool_ref := Some pool;
           pool)
  ;;

  let add_pool ?(pool_size = Config.Database.pool_size) name database_url =
    database_url
    |> connect_or_failwith pool_size (fun pool ->
         if CCOption.is_some (Hashtbl.find_opt pools name)
         then (
           let msg =
             Format.asprintf
               "Failed to create pool: Connection pool with name '%s' exists \
                already"
               name
           in
           Logs.err (fun m -> m "%s" msg);
           raise @@ Exception msg)
         else Hashtbl.add pools name pool)
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
        f connection |> Lwt.map Result.ok)
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
