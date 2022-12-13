type context = (string * string) list

module type Backend = sig
  type actor_role_set
  type actor_spec
  type auth_rule
  type role
  type target_role_set
  type target_spec
  type target_typ
  type 'a authorizable
  type 'b authorizable_target
  type ('rv, 'err) monad = ('rv, 'err) Lwt_result.t

  module Rule : sig
    val find_all : ?ctx:context -> target_spec -> (auth_rule list, string) monad
    val save : ?ctx:context -> auth_rule -> (unit, string) monad
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
        -> target_role_set
        -> Uuid.Target.t
        -> (unit, string) monad

      val mem : ?ctx:context -> Uuid.Target.t -> (bool, string) monad
    end

    val find
      :  ?ctx:context
      -> 'kind
      -> Uuid.Target.t
      -> ('kind authorizable_target, string) monad

    val find_roles
      :  ?ctx:context
      -> Uuid.Target.t
      -> (target_role_set, string) monad

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
    type parent =
      ?ctx:context -> target_spec -> (target_spec option, string) Lwt_result.t

    val register : target_typ -> parent -> (unit, string) result
    val find : target_typ -> parent
  end

  module Rule : sig
    include module type of Rule

    val save_all
      :  ?ctx:context
      -> auth_rule list
      -> (auth_rule list, auth_rule list) Lwt_result.t

    val collect_rules
      :  ?ctx:context
      -> (Action.t * target_spec) list
      -> (auth_rule list, string) monad

    val find_all_exn : ?ctx:context -> target_spec -> auth_rule list Lwt.t
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
      -> ('a -> 'kind authorizable_target)
      -> 'a
      -> ('kind authorizable_target, string) Lwt_result.t

    val find_checker
      :  ?ctx:context
      -> 'a authorizable_target
      -> ('b authorizable -> Action.t -> bool, string) Lwt_result.t

    val find_role_checker
      :  ?ctx:context
      -> target_role_set
      -> ('b authorizable -> Action.t -> bool, string) Lwt_result.t
  end

  val wrap_function
    :  ?ctx:context
    -> (string -> 'etyp)
    -> (Action.t * target_spec) list
    -> ('param -> ('rval, 'etyp) monad)
    -> ('a authorizable -> 'param -> ('rval, 'etyp) monad, string) monad

  val checker_of_effects
    :  ?ctx:context
    -> (Action.t * target_spec) list
    -> 'a authorizable
    -> (unit, string) monad
end
