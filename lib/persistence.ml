type context = (string * string) list

module type Backend = sig
  type 'a actor
  type 'b target
  type actor_spec
  type effect
  type kind
  type query
  type relation
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

      val find_roles : ?ctx:context -> Uuid.Actor.t -> role_set Lwt.t

      val find_by_role
        :  ?ctx:context
        -> ?exclude:roles list
        -> roles
        -> Uuid.Actor.t list Lwt.t

      val find_by_roles
        :  ?ctx:context
        -> ?exclude:roles list
        -> roles list
        -> (roles * Uuid.Actor.t list) list Lwt.t

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

    module Relation : sig
      val upsert
        :  ?ctx:context
        -> ?query:query
        -> kind
        -> kind
        -> (unit, string) monad

      val find_query
        :  ?ctx:context
        -> kind
        -> kind
        -> (query option, string) monad

      val find_rec
        :  ?ctx:context
        -> kind
        -> (kind * kind * query option) list Lwt.t

      val find_effects_rec : ?ctx:context -> effect -> effect list Lwt.t
    end

    val find_rules_of_spec
      :  ?ctx:context
      -> ?any_id:bool
      -> target_spec
      -> rule list Lwt.t

    val exists_for_kind
      :  ?ctx:context
      -> kind
      -> Action.t
      -> 'a actor
      -> Uuid.Target.t list Lwt.t
  end

  val start : ?ctx:context -> unit -> unit Lwt.t
  val find_migrations : unit -> (string * string * string) list
  val find_clean : unit -> context
  val migrate : ?ctx:context -> unit -> unit Lwt.t
  val clean : ?ctx:context -> unit -> unit Lwt.t
end

module type Contract = sig
  include Backend

  module Relation : sig
    val add
      :  ?ctx:context
      -> ?tags:Logs.Tag.set
      -> ?ignore_duplicates:bool
      -> ?to_target:query
      -> target:kind
      -> kind
      -> (unit, string) monad

    val add_multiple
      :  ?ctx:context
      -> ?tags:Logs.Tag.set
      -> ?ignore_duplicates:bool
      -> relation list
      -> (unit, string) monad

    val find
      :  ?ctx:context
      -> ?default:query
      -> target:kind
      -> kind
      -> (relation, string) monad

    val find_opt : ?ctx:context -> target:kind -> kind -> relation option Lwt.t
    val find_rec : ?ctx:context -> kind -> relation list Lwt.t
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
      -> ?any_id:bool
      -> target_spec
      -> ('a actor -> Action.t -> bool, query) result Lwt.t
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
