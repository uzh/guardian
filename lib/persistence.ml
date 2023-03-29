type context = (string * string) list

module type Backend = sig
  type 'a actor
  type 'b target
  type actor_spec
  type effect
  type kind
  type role_set
  type roles
  type rule
  type target_spec
  type validation_set
  type ('rv, 'err) monad = ('rv, 'err) Lwt_result.t

  module Repo : sig
    module Rule : sig
      val find_all : ?ctx:context -> target_spec -> rule list Lwt.t
      val find_all_of_entity : ?ctx:context -> target_spec -> rule list Lwt.t
      val save : ?ctx:context -> rule -> (unit, string) monad
      val delete : ?ctx:context -> rule -> (unit, string) monad
    end

    module Actor : sig
      val create
        :  ?ctx:context
        -> ?owner:Uuid.Actor.t
        -> role_set
        -> Uuid.Actor.t
        -> (unit, string) monad

      val mem : ?ctx:context -> Uuid.Actor.t -> (bool, string) monad

      val find
        :  ?ctx:context
        -> 'kind
        -> Uuid.Actor.t
        -> ('kind actor, string) monad

      val find_roles : ?ctx:context -> Uuid.Actor.t -> (role_set, string) monad

      val find_owner
        :  ?ctx:context
        -> Uuid.Actor.t
        -> (Uuid.Actor.t option, string) monad

      val grant_roles
        :  ?ctx:context
        -> Uuid.Actor.t
        -> role_set
        -> (unit, string) monad

      val revoke_roles
        :  ?ctx:context
        -> Uuid.Actor.t
        -> role_set
        -> (unit, string) monad

      val save_owner
        :  ?ctx:context
        -> ?owner:Uuid.Actor.t
        -> Uuid.Actor.t
        -> (unit, string) monad
    end

    module Target : sig
      val create
        :  ?ctx:context
        -> ?owner:Uuid.Actor.t
        -> kind
        -> Uuid.Target.t
        -> (unit, string) monad

      val mem : ?ctx:context -> Uuid.Target.t -> (bool, string) monad

      val find
        :  ?ctx:context
        -> kind
        -> Uuid.Target.t
        -> (kind target, string) monad

      val find_kind : ?ctx:context -> Uuid.Target.t -> (kind, string) monad

      val find_owner
        :  ?ctx:context
        -> kind
        -> Uuid.Target.t
        -> (Uuid.Actor.t option, string) monad

      val save_owner
        :  ?ctx:context
        -> ?owner:Uuid.Actor.t
        -> Uuid.Target.t
        -> (unit, string) monad
    end
  end

  val find_migrations : unit -> (string * string * string) list
  val find_clean : unit -> (string * string) list
  val migrate : ?ctx:context -> unit -> unit Lwt.t
  val clean : ?ctx:context -> unit -> unit Lwt.t
end

module type Contract = sig
  include Backend

  module Dependency : sig
    type parent_fcn = ?ctx:context -> effect -> (effect option, string) monad

    val register
      :  ?tags:Logs.Tag.set
      -> ?ignore_duplicates:bool
      -> parent:kind
      -> kind
      -> parent_fcn
      -> (unit, string) result

    val find : ?default_fcn:parent_fcn -> parent:kind -> kind -> parent_fcn
    val find_opt : parent:kind -> kind -> parent_fcn option
    val find_all : kind -> parent_fcn list

    val find_all_combined
      :  kind
      -> ?ctx:context
      -> effect
      -> (effect list, string) monad
  end

  module Rule : sig
    include module type of Repo.Rule

    val save_all : ?ctx:context -> rule list -> (rule list, rule list) monad
    val delete_exn : ?ctx:context -> rule -> unit Lwt.t
  end

  module Actor : sig
    include module type of Repo.Actor

    val create
      :  ?ctx:context
      -> ?owner:Uuid.Actor.t
      -> role_set
      -> Uuid.Actor.t
      -> (unit, string) monad

    val mem : ?ctx:context -> Uuid.Actor.t -> (bool, string) monad

    val revoke_role
      :  ?ctx:context
      -> Uuid.Actor.t
      -> roles
      -> (unit, string) monad

    val find_roles_exn : ?ctx:context -> Uuid.Actor.t -> role_set Lwt.t

    val find
      :  ?ctx:context
      -> 'kind
      -> Uuid.Actor.t
      -> ('kind actor, string) monad

    val decorate
      :  ?ctx:context
      -> ('a -> 'kind actor)
      -> 'a
      -> ('kind actor, string) monad
  end

  module Target : sig
    include module type of Repo.Target

    val create
      :  ?ctx:context
      -> ?owner:Uuid.Actor.t
      -> kind
      -> Uuid.Target.t
      -> (unit, string) monad

    val mem : ?ctx:context -> Uuid.Target.t -> (bool, string) monad

    val decorate
      :  ?ctx:context
      -> ('a -> kind target)
      -> 'a
      -> (kind target, string) monad

    val find_checker
      :  ?ctx:context
      -> kind target
      -> ('a actor -> Action.t -> bool, string) monad

    val find_kind_checker
      :  ?ctx:context
      -> kind
      -> ('b actor -> Action.t -> bool, string) monad
  end

  val wrap_function
    :  ?ctx:context
    -> (string -> 'etyp)
    -> validation_set
    -> ('param -> ('rval, 'etyp) monad)
    -> ('a actor -> 'param -> ('rval, 'etyp) monad, string) monad

  val validate
    :  ?ctx:context
    -> ?any_id:bool
    -> (string -> 'etyp)
    -> validation_set
    -> 'a actor
    -> (unit, 'etyp) monad
end
