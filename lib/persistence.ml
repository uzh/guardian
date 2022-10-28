module type Backend_store_s = sig
  type role_set
  type role
  type actor_spec
  type auth_rule
  type 'a authorizable
  type ('rv, 'err) monad = ('rv, 'err) Lwt_result.t

  val find_roles
    :  ?ctx:(string * string) list
    -> Uuidm.t
    -> (role_set, string) monad

  (** [find_rules target_spec] *)
  val find_rules
    :  ?ctx:(string * string) list
    -> actor_spec
    -> (auth_rule list, string) monad

  val save_rule
    :  ?ctx:(string * string) list
    -> auth_rule
    -> (unit, string) monad

  val delete_rule
    :  ?ctx:(string * string) list
    -> auth_rule
    -> (unit, string) monad

  val grant_roles
    :  ?ctx:(string * string) list
    -> Uuidm.t
    -> role_set
    -> (unit, string) monad

  val revoke_roles
    :  ?ctx:(string * string) list
    -> Uuidm.t
    -> role_set
    -> (unit, string) monad

  val create_authorizable
    :  ?ctx:(string * string) list
    -> id:Uuidm.t
    -> ?owner:Uuidm.t
    -> role_set
    -> (unit, string) monad

  val mem_authorizable
    :  ?ctx:(string * string) list
    -> Uuidm.t
    -> (bool, string) monad

  val find_owner
    :  ?ctx:(string * string) list
    -> Uuidm.t
    -> (Uuidm.t option, string) monad

  val save_owner
    :  ?ctx:(string * string) list
    -> Uuidm.t
    -> owner:Uuidm.t
    -> (unit, string) monad

  val find_migrations : unit -> (string * string * string) list
  val migrate : ?ctx:(string * string) list -> unit -> unit Lwt.t
end

module type S = sig
  include Backend_store_s

  val find_authorizable
    :  ?ctx:(string * string) list
    -> typ:'kind
    -> Uuidm.t
    -> ('kind authorizable, string) Lwt_result.t

  val save_rules
    :  ?ctx:(string * string) list
    -> auth_rule list
    -> (auth_rule list, auth_rule list) Lwt_result.t

  val decorate_to_authorizable
    :  ?ctx:(string * string) list
    -> ('a -> 'kind authorizable)
    -> 'a
    -> ('kind authorizable, string) Lwt_result.t

  val find_checker
    :  ?ctx:(string * string) list
    -> 'a authorizable
    -> ('b authorizable -> Action.t -> bool, string) Lwt_result.t

  val find_role_checker
    :  ?ctx:(string * string) list
    -> role_set
    -> ('b authorizable -> Action.t -> bool, string) Lwt_result.t

  val wrap_function
    :  ?ctx:(string * string) list
    -> error:(string -> 'etyp)
    -> effects:(Action.t * actor_spec) list
    -> ('param -> ('rval, 'etyp) monad)
    -> (actor:'a authorizable -> 'param -> ('rval, 'etyp) monad, string) monad

  val revoke_role
    :  ?ctx:(string * string) list
    -> Uuidm.t
    -> role
    -> (unit, string) monad

  val collect_rules
    :  ?ctx:(string * string) list
    -> (Action.t * actor_spec) list
    -> (auth_rule list, string) monad

  val checker_of_effects
    :  ?ctx:(string * string) list
    -> (Action.t * actor_spec) list
    -> actor:'a authorizable
    -> (unit, string) monad

  (** _exn variants of all functions *)
  val find_roles_exn : ?ctx:(string * string) list -> Uuidm.t -> role_set Lwt.t

  val find_rules_exn
    :  ?ctx:(string * string) list
    -> actor_spec
    -> auth_rule list Lwt.t

  val save_rule_exn : ?ctx:(string * string) list -> auth_rule -> unit Lwt.t
  val delete_rule_exn : ?ctx:(string * string) list -> auth_rule -> unit Lwt.t
end
