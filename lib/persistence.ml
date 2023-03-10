type context = (string * string) list

module type Backend = sig
  type actor_role_set
  type actor_spec
  type auth_rule
  type auth_rule_set
  type effect
  type auth_set
  type role
  type target_spec
  type target_typ
  type parent_typ
  type 'a authorizable
  type 'b authorizable_target
  type ('rv, 'err) monad = ('rv, 'err) Lwt_result.t

  module Rule : sig
    type t = actor_spec * Action.t * target_spec

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val compare : t -> t -> int
    val find_all : ?ctx:context -> target_spec -> auth_rule list Lwt.t
    val save : ?ctx:context -> auth_rule -> (unit, string) monad

    (*TODO: only mark as deleted*)
    val delete : ?ctx:context -> auth_rule -> (unit, string) monad
  end

  module Actor : sig
    module Authorizable : sig
      val create
        :  ?ctx:context
        -> ?owner:Uuid.Actor.t
        -> actor_role_set
        -> Uuid.Actor.t
        -> (unit, string) monad

      val mem : ?ctx:context -> Uuid.Actor.t -> (bool, string) monad
    end

    val find
      :  ?ctx:context
      -> 'kind
      -> Uuid.Actor.t
      -> ('kind authorizable, string) monad

    val find_roles
      :  ?ctx:context
      -> Uuid.Actor.t
      -> (actor_role_set, string) monad

    val find_owner
      :  ?ctx:context
      -> Uuid.Actor.t
      -> (Uuid.Actor.t option, string) monad

    val grant_roles
      :  ?ctx:context
      -> Uuid.Actor.t
      -> actor_role_set
      -> (unit, string) monad

    (* TODO: mark as deleted *)
    val revoke_roles
      :  ?ctx:context
      -> Uuid.Actor.t
      -> actor_role_set
      -> (unit, string) monad

    val save_owner
      :  ?ctx:context
      -> ?owner:Uuid.Actor.t
      -> Uuid.Actor.t
      -> (unit, string) monad
  end

  module Target : sig
    module Authorizable : sig
      val create
        :  ?ctx:context
        -> ?owner:Uuid.Actor.t
        -> target_typ
        -> Uuid.Target.t
        -> (unit, string) monad

      val mem : ?ctx:context -> Uuid.Target.t -> (bool, string) monad
    end

    val find
      :  ?ctx:context
      -> target_typ
      -> Uuid.Target.t
      -> (target_typ authorizable_target, string) monad

    val find_kind : ?ctx:context -> Uuid.Target.t -> (target_typ, string) monad

    val find_owner
      :  ?ctx:context
      -> Uuid.Target.t
      -> (Uuid.Actor.t option, string) monad

    val save_owner
      :  ?ctx:context
      -> ?owner:Uuid.Actor.t
      -> Uuid.Target.t
      -> (unit, string) monad
  end

  val find_migrations : unit -> (string * string * string) list
  val find_clean : unit -> (string * string) list
  val migrate : ?ctx:context -> unit -> unit Lwt.t
  val clean : ?ctx:context -> unit -> unit Lwt.t
end

module type Contract = sig
  include Backend

  module Dependency : sig
    type parent = ?ctx:context -> effect -> (effect option, string) Lwt_result.t

    val register
      :  ?tags:Logs.Tag.set
      -> ?ignore_duplicates:bool
      -> target_typ
      -> parent_typ
      -> parent
      -> (unit, string) result

    val find : ?default_fcn:parent -> target_typ -> parent_typ -> parent
    val find_opt : target_typ -> parent_typ -> parent option
    val find_all : target_typ -> parent list

    val find_all_combined
      :  target_typ
      -> ?ctx:context
      -> effect
      -> (effect list, string) Lwt_result.t
  end

  module Rule : sig
    include module type of Rule

    val save_all
      :  ?ctx:context
      -> auth_rule list
      -> (auth_rule list, auth_rule list) Lwt_result.t

    val save_exn : ?ctx:context -> auth_rule -> unit Lwt.t
    val delete_exn : ?ctx:context -> auth_rule -> unit Lwt.t
  end

  module Actor : sig
    include module type of Actor

    val revoke_role
      :  ?ctx:context
      -> Uuid.Actor.t
      -> role
      -> (unit, string) monad

    val find_roles_exn : ?ctx:context -> Uuid.Actor.t -> actor_role_set Lwt.t

    val find_authorizable
      :  ?ctx:context
      -> 'kind
      -> Uuid.Actor.t
      -> ('kind authorizable, string) Lwt_result.t

    val decorate
      :  ?ctx:context
      -> ('a -> 'kind authorizable)
      -> 'a
      -> ('kind authorizable, string) Lwt_result.t
  end

  module Target : sig
    include module type of Target

    val decorate
      :  ?ctx:context
      -> ('a -> target_typ authorizable_target)
      -> 'a
      -> (target_typ authorizable_target, string) Lwt_result.t

    val find_checker
      :  ?ctx:context
      -> target_typ authorizable_target
      -> ('a authorizable -> Action.t -> bool, string) Lwt_result.t

    val find_typ_checker
      :  ?ctx:context
      -> target_typ
      -> ('b authorizable -> Action.t -> bool, string) Lwt_result.t
  end

  val wrap_function
    :  ?ctx:context
    -> (string -> 'etyp)
    -> auth_set
    -> ('param -> ('rval, 'etyp) monad)
    -> ('a authorizable -> 'param -> ('rval, 'etyp) monad, string) monad

  val validate_effects
    :  ?ctx:context
    -> auth_set
    -> 'a authorizable
    -> (unit, string) monad
end
