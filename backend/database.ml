(** The database layer the backends run on.

    Guardian runs its statements on the caller's own context: a pool label (a
    fresh connection per statement), a checked-out connection, or an open
    transaction. With veins, apply {!Of_veins} to your pool; to plug in another
    layer, implement {!Core} and apply {!Make}. Connection pools are not part of
    this; they belong to whoever owns them.

    Every statement declares the capabilities it needs ({!Capabilities}), so
    guardian runs on a role-based veins database as long as the context's role
    has them.

    The callbacks guardian passes to [query] only use the raw connection and
    never call back into the database layer, so a [query] that serialises a
    held connection with a mutex can't deadlock. *)

module type Core = sig
  (** The caller's context. The parameter is the caller's, e.g. a phantom type
      telling transactions apart. *)
  type _ ctx

  (** The parameter of {!default_ctx}'s type. *)
  type default

  (** The context used when a guardian function is called without [?ctx]. *)
  val default_ctx : unit -> default ctx

  (** [label ctx] identifies the database [ctx] points at. Used to key the
      caches and to tag log messages. *)
  val label : _ ctx -> string

  (** [is_transaction ctx] is [true] when [ctx] is inside an open
      transaction. Guardian bypasses its shared caches then, as reads may see
      uncommitted (and possibly rolled back) rows. *)
  val is_transaction : _ ctx -> bool

  (** [after_commit ctx f] runs [f] right away when [ctx] is not a
      transaction, otherwise once that transaction has committed. [f] is
      dropped if the transaction rolls back. Guardian uses it to invalidate its
      caches when written rows become visible to other connections. *)
  val after_commit : _ ctx -> (unit -> unit) -> unit

  (** [query ~needs ctx f] runs [f] on a connection of [ctx] and raises on
      error. [needs] are the capabilities of the statements [f] runs. *)
  val query
    :  needs:Veins.Capability.t list
    -> _ ctx
    -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
    -> 'a Lwt.t

  (** [transaction_iter ~needs ctx fs] runs [fs] in order inside a
      transaction; joins [ctx] when it is one already, so the statements commit
      or roll back with it. *)
  val transaction_iter
    :  needs:Veins.Capability.t list
    -> _ ctx
    -> (Caqti_lwt.connection -> (unit, Caqti_error.t) Lwt_result.t) list
    -> unit Lwt.t
end

module type Sig = sig
  type _ ctx

  val label : ?ctx:_ ctx -> unit -> string
  val is_transaction : ?ctx:_ ctx -> unit -> bool
  val after_commit : ?ctx:_ ctx -> (unit -> unit) -> unit

  val query
    :  needs:Veins.Capability.t list
    -> ?ctx:_ ctx
    -> (Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
    -> 'a Lwt.t

  val find : ?ctx:_ ctx -> ('a, 'b, [< `One ]) Veins.Request.t -> 'a -> 'b Lwt.t

  val find_opt
    :  ?ctx:_ ctx
    -> ('a, 'b, [< `One | `Zero ]) Veins.Request.t
    -> 'a
    -> 'b option Lwt.t

  val collect
    :  ?ctx:_ ctx
    -> ('a, 'b, [< `Many | `One | `Zero ]) Veins.Request.t
    -> 'a
    -> 'b list Lwt.t

  val exec
    :  ?ctx:_ ctx
    -> ('a, unit, [< `Zero ]) Veins.Request.t
    -> 'a
    -> unit Lwt.t

  val populate
    :  needs:Veins.Capability.t list
    -> ?ctx:_ ctx
    -> string
    -> string list
    -> 'a Caqti_type.t
    -> 'a list
    -> unit Lwt.t

  (** [transaction_iter ~needs ?ctx fs] runs [fs] in order inside a
      transaction. When [ctx] already is a transaction it is reused (no nested
      [START TRANSACTION]), so the statements commit or roll back with it. *)
  val transaction_iter
    :  needs:Veins.Capability.t list
    -> ?ctx:_ ctx
    -> (Caqti_lwt.connection -> (unit, Caqti_error.t) Lwt_result.t) list
    -> unit Lwt.t
end

module Make (Core : Core) : Sig with type 'a ctx = 'a Core.ctx = struct
  type 'a ctx = 'a Core.ctx

  (* [?ctx] and the default have different types, so each function matches on
     the option rather than going through a common [resolve]. *)
  let label ?ctx () =
    match ctx with
    | Some ctx -> Core.label ctx
    | None -> Core.label (Core.default_ctx ())
  ;;

  let is_transaction ?ctx () =
    match ctx with
    | Some ctx -> Core.is_transaction ctx
    | None -> Core.is_transaction (Core.default_ctx ())
  ;;

  let after_commit ?ctx f =
    match ctx with
    | Some ctx -> Core.after_commit ctx f
    | None -> Core.after_commit (Core.default_ctx ()) f
  ;;

  let query ~needs ?ctx f =
    match ctx with
    | Some ctx -> Core.query ~needs ctx f
    | None -> Core.query ~needs (Core.default_ctx ()) f
  ;;

  let request ?ctx request f =
    let needs = Veins.Request.needs request |> Veins.Capability.Set.elements in
    query ~needs ?ctx (fun connection ->
      f connection (Veins.Request.caqti request))
  ;;

  let find ?ctx req input =
    request ?ctx req (fun (module Connection : Caqti_lwt.CONNECTION) req ->
      Connection.find req input)
  ;;

  let find_opt ?ctx req input =
    request ?ctx req (fun (module Connection : Caqti_lwt.CONNECTION) req ->
      Connection.find_opt req input)
  ;;

  let collect ?ctx req input =
    request ?ctx req (fun (module Connection : Caqti_lwt.CONNECTION) req ->
      Connection.collect_list req input)
  ;;

  let exec ?ctx req input =
    request ?ctx req (fun (module Connection : Caqti_lwt.CONNECTION) req ->
      Connection.exec req input)
  ;;

  let populate ~needs ?ctx table columns request input =
    query ~needs ?ctx (fun (module Connection : Caqti_lwt.CONNECTION) ->
      Connection.populate
        ~table
        ~columns
        request
        (Caqti_lwt.Stream.of_list input)
      |> Lwt.map Caqti_error.uncongested)
  ;;

  let transaction_iter ~needs ?ctx fns =
    match ctx with
    | Some ctx -> Core.transaction_iter ~needs ctx fns
    | None -> Core.transaction_iter ~needs (Core.default_ctx ()) fns
  ;;
end

(** The {!Core} of a veins pool: guardian runs on [Db]'s contexts and pools.
    Without [?ctx], guardian uses a label context on [Db]'s primary database,
    as [default_role] (required when that database has one URL per role). *)
module Of_veins
    (Db : Veins.Pools_sig.Sig)
    (Default : sig
       val default_role : Veins.Role.t option
     end) :
  Core
  with type 'a ctx = 'a Veins.Ctx.t
   and type default = Veins.Ctx.no_transaction = struct
  type 'a ctx = 'a Veins.Ctx.t
  type default = Veins.Ctx.no_transaction

  let default_ctx () =
    Db.label_ctx
      ?role:Default.default_role
      (Veins.Database.label Db.Config.database)
  ;;

  let label ctx = Veins.Ctx.label ctx |> Veins.Label.value
  let is_transaction = Veins.Ctx.is_transaction
  let after_commit = Veins.Ctx.after_commit
  let query = Db.query
  let transaction_iter = Db.transaction_iter
end
