type actor_spec = [ `Role of Role.t | `Uniq of Uuidm.t ] [@@deriving show]

type auth_rule = actor_spec * Action.t * actor_spec [@@deriving show]

(** actor * actions * target *)
type role_rule = Role.t * Action.t list

(** Produces a function [can : actor -> action -> target -> bool] based on a list
  * of rules defining which roles may perform which actions upon entities of a
  * certain role. *)
let make_checker (rules: role_rule list) = fun actor (action: Action.t) target ->
  let rv =
    Entity.(actor.uuid = target.uuid)
    ||
    Entity.(a_owns_b actor target)
    ||
    List.exists
      (fun (role, authorized_actions) ->
         List.exists ((=) action) authorized_actions && Entity.has_role actor role)
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

module type Authorizable_entity = sig
  type t

  type kind

  (** [to_entity x] converts [x] to a uniquely identifiable entity, complete
    * with roles. The entity may not, however, be converted back into type [t].
   **)
  val to_entity : t -> (kind Entity.t, string) result
end