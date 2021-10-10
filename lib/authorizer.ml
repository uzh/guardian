(** actor * actions * target *)
type role_rule = Role.t * Action.t list

(** Produces a function [can : actor -> action -> target -> bool] based on a list
  * of rules defining which roles may perform which actions upon entities of a
  * certain role. *)
let make_checker (rules: role_rule list) = fun actor (action: Action.t) target ->
  Entity.equal actor target
  ||
  Entity.(a_owns_b actor target)
  ||
  List.exists
    (fun (role, authorized_actions) ->
       List.exists ((=) action) authorized_actions && Entity.has_role actor role)
    rules


module type Authorizable_entity = sig
  type t

  (** [to_entity x] converts [x] to a uniquely identifiable entity, complete
    * with roles. The entity may not, however, be converted back into type [t].
   **)
  val to_entity : t -> Entity.t

  (** [can actor action target] indicates whether the entity [actor] is
    * authorized to perform [action] upon a given [target].
   **)
  val can : Entity.t -> Action.t -> Entity.t -> bool
end