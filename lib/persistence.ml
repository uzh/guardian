module type Backend_store_s = sig
  type role_set
  type role
  type actor_spec
  type auth_rule
  type 'a authorizable

  type ('rv, 'err) monad = ('rv, 'err) Lwt_result.t

  val get_roles : Uuidm.t -> (role_set, string) monad

  val get_perms : actor_spec -> (auth_rule list, string) monad

  val put_perm : auth_rule -> (unit, string) monad

  val delete_perm : auth_rule -> (unit, string) monad

  val grant_roles : Uuidm.t -> role_set -> (unit, string) monad

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

  (** [get_checker auth] Query the database for relevant authorization rules pertaining
      to the [authorizable] [auth] and produce a function to check other [authorizable]s
      for access rights to [auth]. *)
  val get_checker :
    'a authorizable ->
    ('b authorizable -> Action.t -> bool, string) Lwt_result.t

  (** Just like [get_checker], but takes a [role_set] instead of an [authorizable]. *)
  val get_role_checker :
    role_set -> ('a authorizable -> Action.t -> bool, string) Lwt_result.t
  (** _exn variants of all functions *)
  val get_roles_exn : Uuidm.t -> role_set Lwt.t
  val get_perms_exn : actor_spec -> auth_rule list Lwt.t
  val put_perm_exn : auth_rule -> unit Lwt.t
  val delete_perm_exn : auth_rule -> unit Lwt.t
end
