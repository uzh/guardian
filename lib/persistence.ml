module type Backend = sig
  (** Database context the backend runs its statements on: a pool label, a
      held connection or an open transaction. Supplied by the database
      module the backend is built on. *)
  type _ context

  type actor
  type actor_model
  type actor_permission
  type actor_role
  type permission_on_target
  type role
  type role_assignment
  type role_permission
  type target
  type target_entity
  type target_model
  type validation_set

  module Repo : sig
    module Model : sig
      val role : role Caqti_type.t
      val role_assignment : role_assignment Caqti_type.t
    end

    module ActorRole : sig
      val upsert : ?ctx:_ context -> actor_role -> unit Lwt.t

      val find_by_actor
        :  ?ctx:_ context
        -> Uuid.Actor.t
        -> actor_role list Lwt.t

      val find_by_target
        :  ?ctx:_ context
        -> role * Uuid.Target.t
        -> actor_role list Lwt.t

      val find_actors_by_role
        :  ?ctx:_ context
        -> ?exclude:(role * Uuid.Target.t option) list
        -> role * Uuid.Target.t option
        -> Uuid.Actor.t list Lwt.t

      val delete : ?ctx:_ context -> actor_role -> unit Lwt.t

      val permissions_of_actor
        :  ?ctx:_ context
        -> Uuid.Actor.t
        -> permission_on_target list Lwt.t
    end

    module Actor : sig
      val insert : ?ctx:_ context -> actor -> (unit, string) Lwt_result.t
      val mem : ?ctx:_ context -> Uuid.Actor.t -> (bool, string) Lwt_result.t
      val find : ?ctx:_ context -> Uuid.Actor.t -> (actor, string) Lwt_result.t
    end

    module Target : sig
      val insert : ?ctx:_ context -> target -> (unit, string) Lwt_result.t
      val mem : ?ctx:_ context -> Uuid.Target.t -> (bool, string) Lwt_result.t

      val find
        :  ?ctx:_ context
        -> Uuid.Target.t
        -> (target, string) Lwt_result.t

      val find_model
        :  ?ctx:_ context
        -> Uuid.Target.t
        -> (target_model, string) Lwt_result.t

      val promote
        :  ?ctx:_ context
        -> Uuid.Target.t
        -> target_model
        -> unit Lwt.t
    end

    module RolePermission : sig
      val insert
        :  ?ctx:_ context
        -> role_permission
        -> (unit, string) Lwt_result.t

      val delete
        :  ?ctx:_ context
        -> role_permission
        -> (unit, string) Lwt_result.t

      val find_all : ?ctx:_ context -> unit -> role_permission list Lwt.t

      val find_all_of_model
        :  ?ctx:_ context
        -> target_model
        -> role_permission list Lwt.t
    end

    module ActorPermission : sig
      val insert
        :  ?ctx:_ context
        -> actor_permission
        -> (unit, string) Lwt_result.t

      val delete
        :  ?ctx:_ context
        -> actor_permission
        -> (unit, string) Lwt_result.t

      val find_all : ?ctx:_ context -> unit -> actor_permission list Lwt.t

      val find_all_of_entity
        :  ?ctx:_ context
        -> target_entity
        -> actor_permission list Lwt.t
    end

    module RoleAssignment : sig
      val table_name : string
      val sql_select_columns : string list
      val find_request_sql : ?count:bool -> string -> string
      val find_all : ?ctx:_ context -> unit -> role_assignment list Lwt.t

      val find_all_by_role
        :  ?ctx:_ context
        -> role
        -> role_assignment list Lwt.t

      val insert : ?ctx:_ context -> role_assignment list -> unit Lwt.t

      val delete
        :  ?ctx:_ context
        -> ?comment:string
        -> role_assignment
        -> unit Lwt.t
    end

    val validate
      :  ?ctx:_ context
      -> ?any_id:bool
      -> ?target_uuid:Uuid.Target.t
      -> ?model:target_model
      -> Permission.t
      -> actor
      -> bool Lwt.t

    val clear_cache : unit -> unit
  end

  val find_migrations : unit -> (string * string * string) list
  val find_clean : unit -> (string * string) list
  val migrate : ?ctx:_ context -> unit -> unit Lwt.t
  val clean : ?ctx:_ context -> unit -> unit Lwt.t
  val delete : ?ctx:_ context -> unit -> unit Lwt.t
end

module type Contract = sig
  include Backend

  val clear_cache : unit -> unit

  module Actor : sig
    include module type of Repo.Actor

    val decorate
      :  ?ctx:_ context
      -> ('a -> actor)
      -> 'a
      -> (actor, string) Lwt_result.t
  end

  module ActorRole : module type of Repo.ActorRole

  module Target : sig
    include module type of Repo.Target

    val decorate
      :  ?ctx:_ context
      -> ('a -> target)
      -> 'a
      -> (target, string) Lwt_result.t
  end

  module RolePermission : sig
    include module type of Repo.RolePermission

    val insert_all
      :  ?ctx:_ context
      -> role_permission list
      -> (role_permission list, role_permission list) Lwt_result.t
  end

  module ActorPermission : sig
    include module type of Repo.ActorPermission

    val insert_all
      :  ?ctx:_ context
      -> actor_permission list
      -> (actor_permission list, actor_permission list) Lwt_result.t
  end

  module PermissionOnTarget : sig
    val validate_set
      :  ?any_id:bool
      -> permission_on_target list
      -> (string -> 'etyp)
      -> validation_set
      -> actor
      -> (unit, 'etyp) result
  end

  module RoleAssignment : sig
    include module type of Repo.RoleAssignment

    val can_assign_roles : ?ctx:_ context -> role -> role list Lwt.t
  end

  val wrap_function
    :  ?ctx:_ context
    -> (string -> 'etyp)
    -> validation_set
    -> ('param -> ('rval, 'etyp) Lwt_result.t)
    -> (actor -> 'param -> ('rval, 'etyp) Lwt_result.t, string) Lwt_result.t

  val validate
    :  ?ctx:_ context
    -> ?any_id:bool
    -> (string -> 'etyp)
    -> validation_set
    -> actor
    -> (unit, 'etyp) Lwt_result.t
end
