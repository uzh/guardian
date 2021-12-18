module type S = sig
  type role
  type 'a authorizable

  type actor_spec = [ `Role of role | `Uniq of Uuidm.t ] [@@deriving show,ord]

  type auth_rule = actor_spec * Action.t * actor_spec [@@deriving show,ord]
  module Auth_rule_set : Set.S with type elt := auth_rule

  type role_rule = role * Action.t list

  val make_checker :
    role_rule list ->
    'a authorizable ->
    Action.t ->
    'a authorizable ->
    bool

  module type Authorizable_module = sig
    type t
    type kind

    val to_authorizable :
      t -> (kind authorizable, string) Lwt_result.t
  end
end

module Make(R : Role.S)(Authorizable : Authorizable.S with type role = R.t)
: S with type role := R.t and type 'a authorizable := 'a Authorizable.t
= struct
  type actor_spec = [ `Role of R.t | `Uniq of Uuidm.t ] [@@deriving show,ord]

  type auth_rule = actor_spec * Action.t * actor_spec [@@deriving show,ord]

  module Auth_rule_set = Set.Make(struct
      type t = auth_rule
      let compare = compare_auth_rule
    end)

  (** actor * actions * target *)
  type role_rule = R.t * Action.t list

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
    rv

  module type Authorizable_module = sig
    type t

    type kind

    (** [to_authorizable x] converts [x] to a uniquely identifiable object, complete
      * with roles. The [authorizable] may not, however, be converted back into type [t].
    **)
    val to_authorizable : t -> (kind Authorizable.t, string) Lwt_result.t
  end
end