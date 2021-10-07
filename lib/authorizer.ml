(** actor * actions * target *)
type roleRule = Role.t * Action.t list

(** Produces a function [can : actor -> action -> target -> bool] based on a list
  * of rules defining which roles may perform which actions upon entities of a
  * certain role. *)
let makeChecker (rules: roleRule list) = fun actor (action: Action.t) target ->
  Entity.equal actor target
  ||
  Entity.(aOwnsB actor target)
  ||
  List.exists
    (fun (role, authorizedActions) ->
       List.exists ((=) action) authorizedActions && Entity.hasRole actor role)
    rules


module type AuthorizableEntity = sig
  type t

  (** [toEntity x] converts [x] to a uniquely identifiable entity, complete
    * with roles. The entity may not, however, be converted back into type [t].
   **)
  val toEntity : t -> Entity.t

  (** [can actor action target] indicates whether the entity [actor] is
    * authorized to perform [action] upon a given [target].
   **)
  val can : Entity.t -> Action.t -> Entity.t -> bool
end