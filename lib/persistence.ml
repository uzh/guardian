module type Backend_store_s = sig
  type role_set
  type role
  type actor_spec
  type auth_rule
  type 'a authorizable

  type ('rv, 'err) monad = ('rv, 'err) Lwt_result.t

  val get_roles : Uuidm.t -> (role_set, string) monad

  (** [get_perms target_spec] *)
  val get_perms : actor_spec -> (auth_rule list, string) monad

  val put_perm : auth_rule -> (unit, string) monad

  val delete_perm : auth_rule -> (unit, string) monad

  val grant_roles : Uuidm.t -> role_set -> (unit, string) monad

  val revoke_roles : Uuidm.t -> role_set -> (unit, string) monad

  val create_authorizable : id:Uuidm.t -> ?owner:Uuidm.t -> role_set -> (unit, string) monad

  val mem_authorizable : Uuidm.t -> (bool, string) monad

  val get_owner : Uuidm.t -> (Uuidm.t option, string) monad

  val set_owner : Uuidm.t -> owner:Uuidm.t -> (unit, string) monad
end

module type S = sig
  include Backend_store_s

  val get_authorizable : typ:'kind -> Uuidm.t -> ('kind authorizable, string) Lwt_result.t
  val put_perms : auth_rule list -> (auth_rule list, auth_rule list) Lwt_result.t
  val decorate_to_authorizable : ('a -> 'kind authorizable) -> 'a -> ('kind authorizable, string) Lwt_result.t

  val get_checker :
    'a authorizable ->
    ('b authorizable -> Action.t -> bool, string) Lwt_result.t

  val get_role_checker :
    role_set ->
    ('b authorizable -> Action.t -> bool, string) Lwt_result.t

  val wrap_function :
    error:(string -> 'etyp) ->
    effects:(Action.t * actor_spec) list ->
    ('param -> ('rval, 'etyp) monad) ->
    (actor:'a authorizable -> 'param -> ('rval, 'etyp) monad, string) monad

  val revoke_role : Uuidm.t -> role -> (unit, string) monad

  (** _exn variants of all functions *)
  val get_roles_exn : Uuidm.t -> role_set Lwt.t
  val get_perms_exn : actor_spec -> auth_rule list Lwt.t
  val put_perm_exn : auth_rule -> unit Lwt.t
  val delete_perm_exn : auth_rule -> unit Lwt.t
end
