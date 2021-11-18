type actor_spec = [ `Role of Role.t | `Uniq of Uuidm.t ] [@@deriving show,ord]

type auth_rule = actor_spec * Action.t * actor_spec [@@deriving show,ord]

module Auth_rule_set = Set.Make(struct
    type t = auth_rule
    let compare = compare_auth_rule
  end)

(** actor * actions * target *)
type role_rule = Role.t * Action.t list

(** Produces a function [can : actor -> action -> target -> bool] based on a list
  * of rules defining which roles may perform which actions upon entities of a
  * certain role. *)
let make_checker (rules: role_rule list) = fun actor (action: Action.t) target ->
  let rv =
    Authorizable.(actor.uuid = target.uuid)
    ||
    Authorizable.(a_owns_b actor target)
    ||
    List.exists
      (fun (role, authorized_actions) ->
         List.exists ((=) action) authorized_actions && Authorizable.has_role actor role)
      rules
  in
  let () =
    let evt =
      let () = print_endline "Running checker." in
      let status =
        match rv with
        | true -> Logger.Approved
        | false -> Denied
      in
      let time = Option.get @@ Ptime.of_float_s (Unix.gettimeofday()) in
      Logger.make_authorization_event ~actor ~target ~time ~action ~status
    in
    Logger.log evt
  in
  rv

module type Authorizable_module = sig
  type t

  type kind

  (** [to_authorizable x] converts [x] to a uniquely identifiable object, complete
    * with roles. The [authorizable] may not, however, be converted back into type [t].
   **)
  val to_authorizable : t -> (kind Authorizable.t, string) Lwt_result.t
end