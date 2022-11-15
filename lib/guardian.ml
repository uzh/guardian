type context = (string * string) list

module Uuid = Uuid

module type RoleSig = Role.Sig

module Util = struct
  let decompose_variant_string s =
    let open CCString in
    let s = trim s in
    let fmt = format_of_string "`%s (%s@)" in
    try
      Scanf.sscanf s fmt (fun name params ->
        lowercase_ascii name, CCList.map trim (split_on_char ',' params))
    with
    | End_of_file ->
      let fmt = format_of_string "`%s" in
      Scanf.sscanf s fmt (fun name -> lowercase_ascii name, [])
  ;;
end

module Make (A : RoleSig) (T : RoleSig) = struct
  module Uuid = Uuid
  module Action = Action
  module ActorRoleSet : Role_set.S with type elt = A.t = Role_set.Make (A)
  module TargetRoleSet : Role_set.S with type elt = T.t = Role_set.Make (T)

  let to_target actor = actor |> A.name |> T.of_string
  let to_actor target = target |> T.name |> A.of_string

  module Authorizable = struct
    type 'a t =
      { roles : ActorRoleSet.t
      ; owner : Uuid.Actor.t option
      ; uuid : Uuid.Actor.t
      ; typ : 'a
      }
    [@@deriving eq, ord, show, yojson]

    let to_string t = show (fun f _x -> Format.pp_print_string f "") t
    let make ~roles ~typ ?owner uuid = { roles; owner; uuid; typ }

    let a_owns_b a b =
      CCOption.map_or ~default:false (fun b' -> a.uuid = b') b.owner
    ;;

    let has_role t role = ActorRoleSet.mem role t.roles
  end

  module AuthorizableTarget = struct
    type 'a t =
      { owner : Uuid.Actor.t
      ; uuid : Uuid.Target.t
      ; entity : TargetRoleSet.t
      ; typ : 'a
      }
    [@@deriving eq, ord, show, yojson]

    let to_string t = show (fun f _x -> Format.pp_print_string f "") t
    let make ~typ ~owner ~entity uuid = { owner; uuid; typ; entity }
  end

  module Authorizer = struct
    type actor_spec =
      [ `ActorEntity of A.t
      | `Actor of Uuid.Actor.t
      ]
    [@@deriving eq, show, ord]

    let actor_value = function
      | `ActorEntity x -> A.show x
      | `Actor x -> Uuid.Actor.to_string x
    ;;

    type target_spec =
      [ `TargetEntity of T.t
      | `Target of Uuid.Target.t
      ]
    [@@deriving eq, show, ord]

    let target_value = function
      | `TargetEntity x -> T.show x
      | `Target x -> Uuid.Target.to_string x
    ;;

    type auth_rule = actor_spec * Action.t * target_spec
    [@@deriving eq, show, ord]

    (** [action, target] Denotes an effect a function may have on and therefore
        which permissions an actor needs to invoke it. *)
    type effect = Action.t * target_spec [@@deriving eq, show, ord]

    module Effect_set = Set.Make (struct
      type t = effect [@@deriving ord]
    end)

    (** Convenience function to return a [can] function. Takes an optional
        target specification (for error reporting purposes) and a list of
        [guardian] rules of the form [actor, action, target] and returns a
        function that looks like:

        [any_of]: indicates that the checker should pass if any of the rules in
        the list is satisfied. The default behaviour is to only pass if all
        rules are.
        [val can : actor:\[ whatever \] Guard.Authorizable.t -> (unit, string) result] *)
    let checker_of_rules ?(any_of = false) (rules : auth_rule list) =
      let open CCResult in
      fun ~actor ->
        let results =
          CCList.map
            (fun (actor', action, target) ->
              let is_matched =
                match actor' with
                | `Actor uuid ->
                  uuid == actor.Authorizable.uuid
                  && (action == action || action == `Manage)
                | `ActorEntity role ->
                  ActorRoleSet.mem role actor.Authorizable.roles
                  && (action == action || action == `Manage)
              in
              if is_matched
              then Ok ()
              else
                Error
                  (Format.asprintf
                     "Actor %s does not have permission to %s"
                     (Authorizable.to_string actor)
                     (show_effect (action, target))))
            rules
        in
        if any_of
        then
          if CCList.exists (( = ) (Ok ())) results
          then Ok ()
          else
            Error
              (Format.asprintf
                 "Actor %s does not satisfy any of the following rules: %s"
                 (Authorizable.to_string actor)
                 ([%show: auth_rule list] rules))
        else
          CCList.fold_left
            (fun acc x ->
              let* _acc = acc in
              let* _x = x in
              Ok ())
            (Ok ())
            results
    ;;

    module Auth_rule_set = Set.Make (struct
      type t = auth_rule

      let compare = compare_auth_rule
    end)

    module type Actor_module = sig
      type t
      type kind

      (** [to_authorizable x] converts [x] to a uniquely identifiable object,
          complete * with roles. The [authorizable] may not, however, be
          converted back into type [t]. **)
      val to_authorizable
        :  ?ctx:context
        -> t
        -> (kind Authorizable.t, string) Lwt_result.t
    end

    module type Target_module = sig
      type t
      type kind

      (** [to_authorizable x] converts [x] to a uniquely identifiable object,
          complete * with roles. The [authorizable] may not, however, be
          converted back into type [t]. **)
      val to_authorizable
        :  ?ctx:context
        -> t
        -> (kind AuthorizableTarget.t, string) Lwt_result.t
    end
  end

  module type Persistence_s =
    Persistence.Contract
      with type 'a authorizable = 'a Authorizable.t
       and type 'b authorizable_target = 'b AuthorizableTarget.t
       and type actor_role_set = ActorRoleSet.t
       and type actor_spec = Authorizer.actor_spec
       and type target_role_set = TargetRoleSet.t
       and type target_spec = Authorizer.target_spec
       and type auth_rule = Authorizer.auth_rule

  module Make_persistence
    (BES : Persistence.Backend
             with type 'a authorizable = 'a Authorizable.t
              and type 'b authorizable_target = 'b AuthorizableTarget.t
              and type actor_role_set = ActorRoleSet.t
              and type actor_spec = Authorizer.actor_spec
              and type target_role_set = TargetRoleSet.t
              and type target_spec = Authorizer.target_spec
              and type auth_rule = Authorizer.auth_rule
              and type role = A.t) : Persistence_s = struct
    include BES

    (** turn a single argument function returning a [result] into one that
        raises a [Failure] instead *)
    let with_exn ?ctx f name arg =
      match%lwt f ?ctx arg with
      | Ok x -> Lwt.return x
      | Error s -> failwith @@ Format.asprintf "%s failed: %s" name s
    ;;

    let exists_in (auth_rules : auth_rule list) actor action =
      let actor_roles = actor.Authorizable.roles in
      CCList.exists
        (fun (actor', action', _) ->
          match actor' with
          | `Actor id -> actor.uuid = id && action = action'
          | `ActorEntity role ->
            ActorRoleSet.mem role actor_roles
            && (action = action' || action' = `Manage))
        auth_rules
    ;;

    let fold_auth_rules
      :  (auth_rule list, string) result list
      -> (auth_rule list, string) Lwt_result.t
      =
      CCList.fold_left
        (fun acc x ->
          let%lwt acc = acc in
          match acc, x with
          | Ok acc, Ok perms -> Lwt.return_ok (perms @ acc)
          | Error err, _ | _, Error err -> Lwt.return_error err)
        (Lwt.return_ok [])
    ;;

    module Actor = struct
      include BES.Actor

      let revoke_role ?ctx id role =
        Actor.revoke_roles ?ctx id (ActorRoleSet.singleton role)
      ;;

      let find_authorizable ?ctx ~(typ : 'kind) (id : Uuid.Actor.t) =
        let open Lwt_result.Syntax in
        let* mem = Actor.mem_authorizable ?ctx id in
        if mem
        then
          let* roles = Actor.find_roles ?ctx id in
          let* owner = Actor.find_owner ?ctx id in
          Lwt.return (Ok (Authorizable.make ~roles ~typ ?owner id))
        else
          Lwt.return
            (Error
               (Format.asprintf
                  "Authorizable %s doesn't exist."
                  (Uuid.Actor.to_string id)))
      ;;

      (** [save_rules rules] adds all the permissions [rules] to the backend. If
          there is an error at any point, it returns a `result` containing all
          of the items that were not added. *)
      let save_rules ?ctx rules =
        CCList.fold_left
          (fun acc x ->
            match%lwt acc with
            | Ok acc' ->
              (match%lwt BES.Actor.save_rule ?ctx x with
               | Ok () -> Lwt.return_ok (x :: acc')
               | Error _ -> Lwt.return_error [ x ])
            | Error xs -> Lwt.return_error (x :: xs))
          (Lwt.return_ok [])
          rules
      ;;

      (** This convenience function should be used to decorate the
          [to_authorizable] * functions of authorizable modules. The newly
          decorated function connects * to the persistent backend to ensure that
          the authorizable's roles and ownership * are consistent in both
          spaces. *)
      let decorate ?ctx (to_authorizable : 'a -> 'kind Authorizable.t)
        : 'a -> ('kind Authorizable.t, string) Lwt_result.t
        =
       fun x ->
        let open Lwt_result.Syntax in
        let (ent : 'kind Authorizable.t) = to_authorizable x in
        let uuid = ent.uuid in
        let* mem = Actor.mem_authorizable ?ctx ent.uuid in
        if mem
        then
          let* ent' = find_authorizable ?ctx ~typ:ent.typ ent.uuid in
          let roles = ActorRoleSet.union ent.roles ent'.roles in
          let* () = Actor.grant_roles ?ctx uuid roles in
          let* owner =
            match ent.owner, ent'.owner with
            | Some owner, None ->
              let* () = Actor.save_owner ?ctx ent.uuid ~owner in
              Lwt.return_ok (Some owner)
            | None, Some owner -> Lwt.return_ok (Some owner)
            | None, None -> Lwt.return_ok None
            | Some x, Some y when x <> y ->
              (* Still unclear what the desirable behaviour is in this case. *)
              (* Lwt_result.fail( "decorate: both the database and the decorated
                 function \ returned distinct values for the owner of
                 authorizable " ^ Uuid.to_string ent.uuid) *)
              let* () = Actor.save_owner ?ctx ent.uuid ~owner:x in
              Lwt.return_ok (Some x)
            | Some x, Some _ (* when x = y *) -> Lwt.return_ok (Some x)
          in
          Lwt.return_ok Authorizable.{ uuid; roles; owner; typ = ent.typ }
        else
          let* () =
            Actor.create_authorizable ?ctx ~id:uuid ?owner:ent.owner ent.roles
          in
          Lwt.return_ok ent
     ;;

      let find_role_checker ?ctx role_set =
        let open Lwt_result.Syntax in
        let%lwt auth_rules =
          ActorRoleSet.elements role_set
          |> CCList.map (fun r -> `TargetEntity (to_target r))
          |> Lwt_list.map_s (Actor.find_rules ?ctx)
        in
        let* auth_rules = fold_auth_rules auth_rules in
        Lwt.return_ok @@ exists_in auth_rules
      ;;

      let find_roles_exn ?ctx = with_exn Actor.find_roles ?ctx "find_roles_exn"
    end

    module Target = struct
      include BES.Target

      let decorate ?ctx (to_authorizable : 'a -> 'kind AuthorizableTarget.t)
        : 'a -> ('kind AuthorizableTarget.t, string) Lwt_result.t
        =
       fun x ->
        let open Lwt_result.Syntax in
        let (ent : 'kind AuthorizableTarget.t) = to_authorizable x in
        let* mem = mem ?ctx ent.uuid in
        if mem
        then
          let* ent' = find ?ctx ~typ:ent.typ ent.uuid in
          let* owner =
            match ent.owner, ent'.owner with
            | x, y when x <> y ->
              (* Still unclear what the desirable behaviour is in this case. *)
              (* Lwt_result.fail( "decorate: both the database and the decorated
                 function \ returned distinct values for the owner of
                 authorizable " ^ Uuid.to_string ent.uuid) *)
              let* () = save_owner ?ctx ent.uuid ~owner:x in
              Lwt.return_ok x
            | x, _ (* when x = y *) -> Lwt.return_ok x
          in
          AuthorizableTarget.make
            ~typ:ent.typ
            ~owner
            ~entity:ent.entity
            ent.uuid
          |> Lwt.return_ok
        else
          let* () = create ?ctx ~id:ent.uuid ~owner:ent.owner ent.entity in
          Lwt.return_ok ent
     ;;

      let find_checker ?ctx target =
        let open Lwt_result.Syntax in
        let* roles = Target.find_roles ?ctx target.AuthorizableTarget.uuid in
        let%lwt auth_rules =
          TargetRoleSet.elements roles
          |> CCList.map (fun m -> `TargetEntity m)
          |> CCList.cons (`Target target.AuthorizableTarget.uuid)
          |> Lwt_list.map_s (Actor.find_rules ?ctx)
        in
        let* auth_rules = fold_auth_rules auth_rules in
        Lwt.return_ok
        @@ fun actor action ->
        let is_owner =
          target.AuthorizableTarget.owner
          |> Uuid.Actor.equal actor.Authorizable.uuid
        in
        let is_self =
          target.AuthorizableTarget.uuid
          |> Uuid.Target.equal (actor.uuid |> Uuid.target_of_actor)
        in
        if is_self || is_owner then true else exists_in auth_rules actor action
      ;;
    end

    (** [wrap_function ?error ~effects f] produces a wrapped version of [f]
        which checks permissions and gracefully reports authorization errors. *)
    let wrap_function
      ?ctx
      ~(error : string -> 'etyp)
      ~(effects : Authorizer.effect list)
      (f : 'param -> ('rval, 'etyp) Lwt_result.t)
      =
      let open Lwt_result.Syntax in
      let* cans =
        CCList.map
          (fun (action, (target : target_spec)) ->
            let* can =
              match target with
              | `Target uuid ->
                let* auth = Target.find ?ctx ~typ:() uuid in
                Target.find_checker ?ctx auth
              | `TargetEntity role ->
                Actor.find_role_checker
                  ?ctx
                  (ActorRoleSet.singleton (role |> to_actor))
            in
            let can actor =
              if can actor action
              then Ok ()
              else
                Error
                  (Format.asprintf
                     "Entity %s does not have permission to %s target %s."
                     (Authorizable.to_string actor)
                     (Action.to_string action)
                     (Authorizer.target_value target)
                  |> error)
            in
            Lwt.return_ok can)
          effects
        |> CCList.fold_left
             (fun acc x ->
               let* x' = x in
               let* acc' = acc in
               Lwt.return_ok (x' :: acc'))
             (Lwt.return_ok [])
      in
      Lwt.return_ok (fun ~actor param ->
        let* _can =
          CCList.fold_left
            (fun acc can ->
              match acc with
              | Ok _ -> can actor
              | Error _ -> acc)
            (Ok ())
            cans
          |> Lwt.return
        in
        f param)
    ;;

    (* Because of effects that look like [`Action A, `Target X] we need to make
       an extra pass to get all of entity X's roles, because if you have
       permission to do A to one of X's roles, then you should be able to do A
       to X. *)
    let rec expand_effects ?ctx (effects : Authorizer.effect list)
      : (Authorizer.effect list, string) result Lwt.t
      =
      let open Lwt_result.Syntax in
      let%lwt lsls =
        Lwt_list.map_s
          (fun effect ->
            match effect with
            | action, `Target (x : Uuid.Target.t) ->
              let* roles = Actor.find_roles ?ctx (x |> Uuid.actor_of_target) in
              let* set =
                CCList.map
                  (fun role ->
                    expand_effects
                      ?ctx
                      [ action, `TargetEntity (role |> to_target) ])
                  (ActorRoleSet.elements roles)
                |> CCList.fold_left
                     (fun rv effect ->
                       let* rv = rv in
                       let* effect = effect in
                       let set' =
                         CCList.fold_left
                           (fun acc x -> Authorizer.Effect_set.add x acc)
                           rv
                           effect
                       in
                       Lwt.return_ok set')
                     (Lwt.return_ok (Authorizer.Effect_set.singleton effect))
              in
              Lwt_result.return (Authorizer.Effect_set.elements set)
            | x -> Lwt.return_ok [ x ])
          effects
      in
      CCList.fold_left
        (fun x acc ->
          match acc, x with
          | Error _, _ -> acc
          | Ok _, Error _ -> x
          | Ok acc', Ok x' -> Ok (x' @ acc'))
        (Ok [])
        lsls
      |> Lwt.return
    ;;

    (** [collect_rules e] Query the database for a list of rules pertaining to
        the effects [e]. *)
    let collect_rules ?ctx (effects : Authorizer.effect list) =
      Lwt_result.map_error
        (Format.asprintf
           "Failed to collect rules for effects list %s. Error message: %s"
           ([%show: Authorizer.effect list] effects))
      @@
      let open Lwt_result.Syntax in
      let* effects = expand_effects ?ctx effects in
      let%lwt results =
        Lwt_list.map_s
          (fun (action, target) ->
            let* rules = Actor.find_rules ?ctx target in
            CCList.filter
              (fun (_actor, action', _target) ->
                action = action' || action' = `Manage)
              rules
            |> Lwt_result.return)
          effects
      in
      let* rules =
        CCList.fold_left
          (fun acc (x : (auth_rule list, string) result) ->
            let open CCResult in
            both acc x >|= CCFun.uncurry CCList.append)
          (Ok [])
          results
        |> Lwt_result.lift
      in
      CCList.fold_right
        Authorizer.Auth_rule_set.add
        rules
        Authorizer.Auth_rule_set.empty
      |> Authorizer.Auth_rule_set.elements
      |> Lwt_result.return
    ;;

    let checker_of_effects ?ctx effects ~actor =
      Lwt_list.fold_left_s
        (fun acc effect ->
          let open Lwt_result.Syntax in
          let* () = Lwt_result.lift acc in
          let* rules = collect_rules ?ctx [ effect ] in
          Authorizer.checker_of_rules ~any_of:true rules ~actor
          |> CCResult.map_err (fun err ->
               Format.asprintf
                 "Actor %s does not have permission to %s target %s. Error \
                  message: %s"
                 (Authorizable.to_string actor)
                 (Action.show (fst effect))
                 ([%show: Authorizer.target_spec] (snd effect))
                 err)
          |> Lwt_result.lift)
        (Ok ())
        effects
    ;;

    let find_rules_exn ?ctx = with_exn Actor.find_rules ?ctx "find_rules_exn"
    let save_rule_exn ?ctx = with_exn Actor.save_rule ?ctx "save_rule_exn"
    let delete_rule_exn ?ctx = with_exn Actor.delete_rule ?ctx "delete_rule_exn"
  end
end
