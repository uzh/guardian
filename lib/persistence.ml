type context = (string * string) list

module type Backend = sig
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
      val upsert : ?ctx:(string * string) list -> actor_role -> unit Lwt.t

      val find_by_actor
        :  ?ctx:(string * string) list
        -> Uuid.Actor.t
        -> actor_role list Lwt.t

      val find_by_target
        :  ?ctx:(string * string) list
        -> role * Uuid.Target.t
        -> actor_role list Lwt.t

      val find_actors_by_role
        :  ?ctx:(string * string) list
        -> ?exclude:(role * Uuid.Target.t option) list
        -> role * Uuid.Target.t option
        -> Uuid.Actor.t list Lwt.t

      val delete : ?ctx:(string * string) list -> actor_role -> unit Lwt.t

      val permissions_of_actor
        :  ?ctx:context
        -> Uuid.Actor.t
        -> permission_on_target list Lwt.t
    end

    module Actor : sig
      val insert : ?ctx:context -> actor -> (unit, string) Lwt_result.t
      val mem : ?ctx:context -> Uuid.Actor.t -> (bool, string) Lwt_result.t
      val find : ?ctx:context -> Uuid.Actor.t -> (actor, string) Lwt_result.t
    end

    module Target : sig
      val insert : ?ctx:context -> target -> (unit, string) Lwt_result.t
      val mem : ?ctx:context -> Uuid.Target.t -> (bool, string) Lwt_result.t
      val find : ?ctx:context -> Uuid.Target.t -> (target, string) Lwt_result.t

      val find_model
        :  ?ctx:context
        -> Uuid.Target.t
        -> (target_model, string) Lwt_result.t

      val promote : ?ctx:context -> Uuid.Target.t -> target_model -> unit Lwt.t
    end

    module RolePermission : sig
      val insert
        :  ?ctx:context
        -> role_permission
        -> (unit, string) Lwt_result.t

      val delete
        :  ?ctx:context
        -> role_permission
        -> (unit, string) Lwt_result.t

      val find_all : ?ctx:context -> unit -> role_permission list Lwt.t

      val find_all_of_model
        :  ?ctx:context
        -> target_model
        -> role_permission list Lwt.t
    end

    module ActorPermission : sig
      val insert
        :  ?ctx:context
        -> actor_permission
        -> (unit, string) Lwt_result.t

      val delete
        :  ?ctx:context
        -> actor_permission
        -> (unit, string) Lwt_result.t

      val find_all : ?ctx:context -> unit -> actor_permission list Lwt.t

      val find_all_of_entity
        :  ?ctx:context
        -> target_entity
        -> actor_permission list Lwt.t
    end

    module RoleAssignment : sig
      val table_name : string
      val sql_select_columns : string list
      val find_request_sql : ?count:bool -> string -> string

      val find_all
        :  ?ctx:(string * string) list
        -> unit
        -> role_assignment list Lwt.t

      val find_all_by_role
        :  ?ctx:(string * string) list
        -> role
        -> role_assignment list Lwt.t

      val insert
        :  ?ctx:(string * string) list
        -> role_assignment list
        -> unit Lwt.t

      val delete
        :  ?ctx:(string * string) list
        -> ?comment:string
        -> role_assignment
        -> unit Lwt.t
    end

    val validate
      :  ?ctx:context
      -> ?any_id:bool
      -> ?target_uuid:Uuid.Target.t
      -> ?model:target_model
      -> Permission.t
      -> actor
      -> bool Lwt.t

    val clear_cache : unit -> unit
  end

  val start : ?ctx:context -> unit -> unit Lwt.t [@@deprecated]
  val find_migrations : unit -> (string * string * string) list
  val find_clean : unit -> context
  val migrate : ?ctx:context -> unit -> unit Lwt.t
  val clean : ?ctx:context -> unit -> unit Lwt.t
  val delete : ?ctx:(string * string) list -> unit -> unit Lwt.t
end

module type Contract = sig
  include Backend

  val clear_cache : unit -> unit

  module Actor : sig
    include module type of Repo.Actor

    val decorate
      :  ?ctx:context
      -> ('a -> actor)
      -> 'a
      -> (actor, string) Lwt_result.t
  end

  module ActorRole : module type of Repo.ActorRole

  module Target : sig
    include module type of Repo.Target

    val decorate
      :  ?ctx:context
      -> ('a -> target)
      -> 'a
      -> (target, string) Lwt_result.t
  end

  module RolePermission : sig
    include module type of Repo.RolePermission

    val insert_all
      :  ?ctx:context
      -> role_permission list
      -> (role_permission list, role_permission list) Lwt_result.t
  end

  module ActorPermission : sig
    include module type of Repo.ActorPermission

    val insert_all
      :  ?ctx:context
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

    val can_assign_roles
      :  ?ctx:(string * string) list
      -> role
      -> role list Lwt.t
  end

  val wrap_function
    :  ?ctx:context
    -> (string -> 'etyp)
    -> validation_set
    -> ('param -> ('rval, 'etyp) Lwt_result.t)
    -> (actor -> 'param -> ('rval, 'etyp) Lwt_result.t, string) Lwt_result.t

  val validate
    :  ?ctx:context
    -> ?any_id:bool
    -> (string -> 'etyp)
    -> validation_set
    -> actor
    -> (unit, 'etyp) Lwt_result.t
end
