module Uuidm = struct
  include Uuidm

  let to_yojson t =
    `String (to_string t)

  let of_yojson = function
    | `String s ->
      of_string s
      |> begin function
        | Some x -> Ok x
        | None -> Error("Invalid UUID: " ^ s)
      end
    | _ -> raise (Invalid_argument "")
end

module type Role_s = Role.S

module Make(R : Role.S) = struct
  module Uuid = Uuid
  
  module Action = Action

  module Role_set : Role_set.S with type elt = R.t = Role_set.Make(R)

  module Authorizable = struct
    type 'a t =
      { roles: Role_set.t
      ; owner: unit t option
      ; uuid: Uuid.t
      ; typ: 'a
      } [@@deriving eq,ord,show,yojson]
  
    let make ~roles ~typ ?owner uuid =
      { roles
      ; owner
      ; uuid
      ; typ
      }
  
    let a_owns_b a b =
      Option.map (fun b' -> a.uuid = b'.uuid) b.owner = Some true
  
    let has_role t role =
      Role_set.mem role t.roles
  end

  module Authorizer = struct
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

  module type Persistence_s = (Persistence.S
    with type 'a authorizable = 'a Authorizable.t
    and type role_set = Role_set.t
    and type actor_spec = Authorizer.actor_spec
    and type auth_rule = Authorizer.auth_rule)

  module Make_persistence(BES : Persistence.Backend_store_s
        with type 'a authorizable = 'a Authorizable.t
        and type role_set = Role_set.t
        and type actor_spec = Authorizer.actor_spec
        and type auth_rule = Authorizer.auth_rule)
    : Persistence_s
  = struct
    include BES
    
    let ( let* ) = Lwt_result.bind

    let rec get_typeless_authorizable id : (unit Authorizable.t, string) Lwt_result.t =
      let* mem = BES.mem_authorizable id in
      if mem
        then
          let* roles = BES.get_roles id in
          let* owner_id = BES.get_owner id in
          let* owner =
            match owner_id with
            | Some owner_id' ->
              let* x = get_typeless_authorizable owner_id' in
              Lwt.return_ok(Some x)
            | None ->
              Lwt.return_ok(None)
          in
          Lwt.return_ok(Authorizable.make ~roles ~typ:() ?owner id)
        else
          Lwt.return(Error(Printf.sprintf "Authorizable %s doesn't exist." (Uuidm.to_string id)))
    let get_authorizable ~(typ : 'kind) id =
      let* mem = BES.mem_authorizable id in
      if mem
      then
        let* roles = BES.get_roles id in
        let* owner_id = BES.get_owner id in
        let* owner =
          match owner_id with
          | Some owner_id' -> Lwt_result.map (Option.some) (get_typeless_authorizable owner_id')
          | None -> Lwt_result.return(None)
        in
        Lwt.return(Ok(Authorizable.make ~roles ~typ ?owner id))
      else
        Lwt.return(Error(Printf.sprintf "Authorizable %s doesn't exist." (Uuidm.to_string id)))

    (** [put_perms perms] adds all the permissions [perms] to the backend. If
        there is an error at any point, it returns a `result` containing all of
        the items that were not added. *)
    let put_perms perms =
      List.fold_left
        (fun acc x ->
            match%lwt acc with
            | Ok acc' ->
              begin match%lwt BES.put_perm x with
                | Ok() -> Lwt.return_ok(x :: acc')
                | Error _ -> Lwt.return_error[x]
              end
            | Error xs ->
              Lwt.return_error(x :: xs)
        )
        (Lwt.return_ok [])
        perms

    (** This convenience function should be used to decorate the [to_authorizable]
      * functions of authorizable modules. The newly decorated function connects
      * to the persistent backend to ensure that the authorizable's roles and ownership
      * are consistent in both spaces.
    *)
    let decorate_to_authorizable
        (to_authorizable : 'a -> 'kind Authorizable.t)
      : 'a -> ('kind Authorizable.t, string) Lwt_result.t =
      fun x ->
      let (ent : 'kind Authorizable.t) = to_authorizable x in
      let uuid = ent.uuid in
      let* mem = BES.mem_authorizable ent.uuid in
      if mem
      then
        let* ent' = get_authorizable ~typ:ent.typ ent.uuid in
        let roles = Role_set.union ent.roles ent'.roles in
        let* () = BES.grant_roles uuid roles in
        let* owner =
          match ent.owner, ent'.owner with
          | Some owner, None ->
            let* () = BES.set_owner ent.uuid ~owner:owner.uuid in
            Lwt.return_ok(Some owner)
          | None, Some owner ->
            Lwt.return_ok(Some owner)
          | None, None ->
            Lwt.return_ok None
          | Some x, Some y when x <> y ->
            Lwt_result.fail(
              "decorate_to_authorizable: both the database and the decorated function \
                returned distinct values for the owner of authorizable "
              ^ Uuidm.to_string ent.uuid)
          | Some x, Some _ (* when x = y *) ->
            Lwt.return_ok(Some x)
        in
        Lwt.return_ok Authorizable.{uuid; roles; owner; typ = ent.typ}
      else
        let* () = BES.create_authorizable ~id:uuid ent.roles in
        Lwt.return_ok ent

    let get_checker authorizable =
      let%lwt auth_rules =
        Role_set.elements authorizable.Authorizable.roles
        |> List.map (fun r -> `Role r)
        |> List.cons (`Uniq authorizable.Authorizable.uuid)
        |> Lwt_list.map_s BES.get_perms
      in
      let* auth_rules =
        List.fold_left
          (fun acc x ->
            let%lwt acc = acc in
            match acc, x with
            | Ok acc, Ok perms ->
              Lwt.return_ok(perms @ acc)
            | Error err, _
            | _, Error err ->
              Lwt.return_error err)
          (Lwt.return_ok [])
          auth_rules
      in
      Lwt.return_ok @@
      fun actor action ->
        let is_owner =
          match authorizable.Authorizable.owner with
          | Some x -> Authorizable.(actor.uuid = x.uuid)
          | None -> false
        in
        let is_self = Authorizable.(actor.uuid = authorizable.uuid) in
        if is_self || is_owner
        then true
        else
          let actor_roles = actor.Authorizable.roles in
          List.exists
            (fun (actor', action', _) ->
                match actor' with
                | `Uniq id ->
                  actor.uuid = id && action = action'
                | `Role role ->
                  Role_set.mem role actor_roles && (action = action' || action' = `Manage)
            )
            auth_rules

    let exceptionalize1 f name =
      fun arg ->
        let%lwt res = f arg in
        match res with
        | Ok x -> Lwt.return x
        | Error s -> raise(Failure(name ^ " failed: " ^ s))
    let get_roles_exn =
      exceptionalize1 BES.get_roles "get_roles_exn"
    let get_perms_exn =
      exceptionalize1 BES.get_perms "get_perms_exn"
    let put_perm_exn =
      exceptionalize1 BES.put_perm "put_perm_exn"
    let delete_perm_exn =
      exceptionalize1 BES.delete_perm "delete_perm_exn"
  end
end
