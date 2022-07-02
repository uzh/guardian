module Uuidm = struct
  include Uuid
end

module Util = struct
  let decompose_variant_string s =
    let s = String.trim s in
    let fmt = format_of_string "`%s (%s@)" in
    try
      Scanf.sscanf
        s
        fmt
        (fun name params -> String.(lowercase_ascii name, List.map trim (split_on_char ',' params)))
    with
      | End_of_file ->
        let fmt = format_of_string "`%s" in
        Scanf.sscanf s fmt (fun name -> String.lowercase_ascii name, [])
end

module type Role_s = Role.S

module Make(R : Role.S) = struct
  module Uuid = Uuid

  module Action = Action

  module Role_set : Role_set.S with type elt = R.t = Role_set.Make(R)

  module Authorizable = struct
    type 'a t =
      { roles: Role_set.t
      ; owner: Uuid.t option
      ; uuid: Uuid.t
      ; typ: 'a
      } [@@deriving eq,ord,show,yojson]
    
    let to_string t =
      show (fun f _x -> Format.pp_print_string f "") t
  
    let make ~roles ~typ ?owner uuid =
      { roles
      ; owner
      ; uuid
      ; typ
      }
  
    let a_owns_b a b =
      Option.map (fun b' -> a.uuid = b') b.owner = Some true
  
    let has_role t role =
      Role_set.mem role t.roles
  end

  module Authorizer = struct
    type actor_spec = [ `Role of R.t | `Uniq of Uuidm.t ] [@@deriving show,ord]
  
    type auth_rule = actor_spec * Action.t * actor_spec [@@deriving show,ord]

    (** [action, target] Denotes an effect a function may have on and therefore
        which permissions an actor needs to invoke it. *)
    type effect = Action.t * actor_spec [@@deriving show,ord]

    module Effect_set = Set.Make(struct type t = effect [@@deriving ord] end)

    (** Convenience function to return a [can] function. Takes an optional target
      specification (for error reporting purposes) and a list of [ocaml_authorize]
      rules of the form [actor, action, target] and returns a function that looks
      like:
      [val can : actor:\[ whatever \] Ocauth.Authorizable.t -> (unit, string) result] *)
    let checker_of_rules (rules : auth_rule list) =
      let ( let* ) = CCResult.( let* ) in
      fun ~actor ->
        CCList.fold_left
          (fun acc (actor', action, target) ->
            let* _rv = acc in
            let is_matched =
              match actor' with
              | `Uniq uuid ->
                uuid = actor.Authorizable.uuid
                && (action = action || action = `Manage)
              | `Role role ->
                Role_set.mem role actor.Authorizable.roles
                && (action = action || action = `Manage)
            in
            if is_matched
            then Ok ()
            else
              Error
                (Format.asprintf
                    "Actor %s does not have permission to %s"
                    (Authorizable.to_string actor)
                    (show_effect (action, target))))
          (Ok ())
          rules
  
    module Auth_rule_set = Set.Make(struct
        type t = auth_rule
        let compare = compare_auth_rule
      end)
  
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
        and type auth_rule = Authorizer.auth_rule
        and type role = R.t)
    : Persistence_s
  = struct
    include BES
    
    let ( let* ) = Lwt_result.bind

    let revoke_role id role =
      revoke_roles id (Role_set.singleton role)

    let get_authorizable ~(typ : 'kind) id =
      let* mem = BES.mem_authorizable id in
      if mem
      then
        let* roles = BES.get_roles id in
        let* owner = BES.get_owner id in
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
            let* () = BES.set_owner ent.uuid ~owner in
            Lwt.return_ok(Some owner)
          | None, Some owner ->
            Lwt.return_ok(Some owner)
          | None, None ->
            Lwt.return_ok None
          | Some x, Some y when x <> y ->
            (** Still unclear what the desirable behaviour is in this case. *)
            (* Lwt_result.fail(
              "decorate_to_authorizable: both the database and the decorated function \
                returned distinct values for the owner of authorizable "
              ^ Uuidm.to_string ent.uuid) *)
            let* () = BES.set_owner ent.uuid ~owner:x in
            Lwt.return_ok(Some x)
          | Some x, Some _ (* when x = y *) ->
            Lwt.return_ok(Some x)
        in
        Lwt.return_ok Authorizable.{uuid; roles; owner; typ = ent.typ}
      else
        let* () = BES.create_authorizable ~id:uuid ?owner:ent.owner ent.roles in
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
          | Some x -> Authorizable.(actor.uuid = x)
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
    
    let get_role_checker role_set =
      let%lwt auth_rules =
        Role_set.elements role_set
        |> List.map (fun r -> `Role r)
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

    (** [wrap_function ?error ~effects f] produces a wrapped version of [f] which
        checks permissions and gracefully reports authorization errors. *)
    let wrap_function
        ~(error: string -> 'etyp)
        ~effects
        (f: 'param -> ('rval, 'etyp) Lwt_result.t) =
      let* cans =
        List.map
          (fun (action, target) ->
            let* can =
              match target with
              | `Uniq uuid ->
                let* authorizable = get_authorizable ~typ:() uuid in
                get_checker authorizable
              | `Role role ->
                get_role_checker (Role_set.singleton role)
            in
            let can actor =
              if can actor action
              then Ok()
              else Error(
                Format.asprintf
                  "Entity %s does not have permission to %s target %s."
                  (Authorizable.to_string actor)
                  (Action.to_string action)
                  (Authorizer.show_actor_spec target)
                |> error)
            in
            Lwt.return_ok can)
          effects
        |> List.fold_left
            (fun acc x ->
              let* x' = x in
              let* acc' = acc in
              Lwt.return_ok(x' :: acc'))
            (Lwt.return_ok[])
      in
      Lwt.return_ok(
        fun ~actor param ->
          let* _can =
            List.fold_left
              (fun acc can ->
                match acc with
                | Ok _ -> can actor
                | Error _ -> acc)
              (Ok())
              cans
            |> Lwt.return
          in
          f param)

    (* Because of effects that look like [`Action A, `Uniq X] we need to make an extra pass
       to get all of entity X's roles, because if you have permission to do A to one of
        X's roles, then you should be able to do A to X. *)
    let rec expand_effects (effects : Authorizer.effect list): (Authorizer.effect list, string) result Lwt.t =
      let open Lwt_result.Syntax in
      let lsls: (Authorizer.effect list, string) result Lwt.t list =
        List.map
          (fun effect ->
            match effect with
            | action, `Uniq x ->
              let* roles = get_roles x in
              let* set =
                List.map
                  (fun role ->
                    expand_effects [action, `Role role])
                  (Role_set.elements roles)
                |> List.fold_left
                    (fun rv effect ->
                      let* rv = rv in
                      let* effect = effect in
                      let set' = List.fold_left (fun acc x -> Authorizer.Effect_set.add x acc) rv effect in
                      Lwt.return_ok set')
                    (Lwt.return_ok(Authorizer.Effect_set.singleton effect))
              in
              Lwt_result.return(Authorizer.Effect_set.elements set)
            | x ->
              Lwt.return_ok[x]
            )
          effects
      in
      List.fold_left
        (fun x acc ->
          let* x' = x in
          let* acc' = acc in
          Lwt.return_ok(x' @ acc'))
        (Lwt.return_ok [])
        lsls

    (** [collect_rules e] Query the database for a list of rules pertaining to the
      effects [e]. Note that due to the implemented behaviour of
      [Ocauth.Persistence.get_perms], this function may not behave as expected for
      effects targeting a [`Uniq] entity. It will only collect rules that
      explicitly target that particular entity, NOT rules pertaining to that
      entity's roles. E.g if you have some [subject s] with the [`Subject] role
      and UUID [id1], and a rules list that looks like this:

      {[
        1: `Role `Operator, `Update, `Role `Subject
        2: `Uniq id2, `Update, `Uniq id1
      ]}

      Then this function invoked as [collect_rules \[`Update, `Uniq id1\]] will
      only return row 2. *)
    let collect_rules (effects : Authorizer.effect list) =
      let open Lwt_result.Syntax in
      let* effects = expand_effects effects in
      let results =
        CCList.map
          (fun (action, target) ->
            let* rules = get_perms target in
            CCList.filter (fun (_actor, action', _target) -> action = action' || action' = `Manage) rules
            |> Lwt_result.return)
          effects
      in
      let* rules =
        CCList.fold_left
          (fun acc x ->
            let* acc = acc in
            let* x = x in
            Lwt_result.return (CCList.append acc x))
          (Lwt_result.return [])
          results
      in
      CCList.fold_right
        Authorizer.Auth_rule_set.add
        rules
        Authorizer.Auth_rule_set.empty
      |> Authorizer.Auth_rule_set.elements
      |> Lwt_result.return
    ;;

    (** turn a single argument function returning a [result] into one that raises
    a [Failure] instead *)
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
