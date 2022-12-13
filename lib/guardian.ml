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

  type target_typ = T.t

  module Authorizable = struct
    type 'a t =
      { roles : ActorRoleSet.t
      ; owner : Uuid.Actor.t option
      ; uuid : Uuid.Actor.t
      ; typ : 'a
      }
    [@@deriving eq, ord, show, yojson]

    let to_string t = show (fun f _ -> Format.pp_print_string f "") t
    let make ?owner roles typ uuid = { roles; owner; uuid; typ }

    let a_owns_b a b =
      CCOption.map_or ~default:false (fun b' -> a.uuid = b') b.owner
    ;;

    let has_role { roles; _ } = CCFun.flip ActorRoleSet.mem roles
  end

  module AuthorizableTarget = struct
    type 'a t =
      { uuid : Uuid.Target.t
      ; owner : Uuid.Actor.t option
      ; entity : TargetRoleSet.t
      ; typ : 'a
      }
    [@@deriving eq, ord, show, yojson]

    let to_string t = show (fun f _ -> Format.pp_print_string f "") t
    let make ?owner entity typ uuid = { owner; uuid; typ; entity }
  end

  module Authorizer = struct
    module Actor = struct
      type spec =
        [ `ActorEntity of A.t
        | `Actor of A.t * Uuid.Actor.t
        ]
      [@@deriving eq, show, ord]

      let value = A.show
    end

    module Target = struct
      type spec =
        [ `TargetEntity of T.t
        | `Target of T.t * Uuid.Target.t
        ]
      [@@deriving eq, show, ord]

      let value = T.show
    end

    type auth_rule = Actor.spec * Action.t * Target.spec
    [@@deriving eq, show, ord]

    (** [action, target] Denotes an effect a function may have on and therefore
        which permissions an actor needs to invoke it. *)
    type effect = Action.t * Target.spec [@@deriving eq, show, ord]

    module EffectSet = Set.Make (struct
      type t = effect [@@deriving ord]
    end)

    module Dependency = struct
      module Map = CCMap.Make (T)

      type parent =
        ?ctx:context -> Target.spec -> (Target.spec option, string) Lwt_result.t

      let registered : parent Map.t ref = ref Map.empty

      let register typ (parent_fcn : parent) =
        let found = Map.find_opt typ !registered in
        match found with
        | Some _ ->
          Error
            (Format.asprintf
               "Found duplicate migration '%s', ignoring it"
               ([%show: T.t] typ))
        | None ->
          registered := Map.add typ parent_fcn !registered;
          Ok ()
      ;;

      let register_all dependencies =
        let open CCResult in
        dependencies
        |> CCList.map (CCFun.uncurry register)
        |> flatten_l
        >|= fun (_ : unit list) -> ()
      ;;

      let find (typ : T.t)
        :  ?ctx:context -> Target.spec
        -> (Target.spec option, string) Lwt_result.t
        =
        let default_fcn ?ctx _ =
          let _ = ctx in
          Lwt_result.return None
        in
        Map.find_opt typ !registered
        |> function
        | Some parent_fcn -> parent_fcn
        | None -> default_fcn
      ;;
    end

    (** Convenience function to return a [can] function. Takes an optional
        target specification (for error reporting purposes) and a list of
        [guardian] rules of the form [actor, action, target] and returns a
        function that looks like:

        [any_of]: indicates that the checker should pass if any of the rules in
        the list is satisfied. The default behaviour is to only pass if all
        rules are.
        [val can : actor:\[ whatever \] Guard.Authorizable.t -> (unit, string) result] *)
    let checker_of_rules ?(any_of = false) actor (rules : auth_rule list) =
      let open CCResult in
      let results =
        rules
        |> CCList.map (fun (actor', action, target) ->
             let is_matched = function
               | `Actor (role, uuid) ->
                 uuid = actor.Authorizable.uuid
                 && ActorRoleSet.mem role actor.Authorizable.roles
               | `ActorEntity role ->
                 ActorRoleSet.mem role actor.Authorizable.roles
             in
             if is_matched actor'
             then Ok ()
             else
               Error
                 (Format.asprintf
                    "Actor %s does not have permission to %s"
                    (Authorizable.to_string actor)
                    (show_effect (action, target))))
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
        results
        |> CCResult.flatten_l
        |> CCResult.map (fun (_ : unit list) -> ())
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
       and type actor_spec = Authorizer.Actor.spec
       and type target_role_set = TargetRoleSet.t
       and type target_spec = Authorizer.Target.spec
       and type target_typ = TargetRoleSet.elt
       and type auth_rule = Authorizer.auth_rule

  module Make_persistence
    (Backend : Persistence.Backend
                 with type 'a authorizable = 'a Authorizable.t
                  and type 'b authorizable_target = 'b AuthorizableTarget.t
                  and type actor_role_set = ActorRoleSet.t
                  and type actor_spec = Authorizer.Actor.spec
                  and type target_role_set = TargetRoleSet.t
                  and type target_spec = Authorizer.Target.spec
                  and type auth_rule = Authorizer.auth_rule
                  and type target_typ = TargetRoleSet.elt
                  and type role = A.t) : Persistence_s = struct
    include Backend
    module Dependency = Authorizer.Dependency

    (** turn a single argument function returning a [result] into one that
        raises a [Failure] instead *)
    let with_exn ?ctx f name arg =
      match%lwt f ?ctx arg with
      | Ok x -> Lwt.return x
      | Error s -> failwith @@ Format.asprintf "%s failed: %s" name s
    ;;

    let exists_in (auth_rules : auth_rule list) actor action =
      CCList.exists
        (fun (actor', action', _) ->
          match actor' with
          | `Actor (role, id) ->
            actor.Authorizable.uuid = id
            && action = action'
            && ActorRoleSet.mem role actor.Authorizable.roles
          | `ActorEntity role ->
            ActorRoleSet.mem role actor.Authorizable.roles
            && (action = action' || action' = `Manage))
        auth_rules
    ;;

    module Actor = struct
      module AuthorizableActor = Authorizable
      include Actor

      let revoke_role ?ctx id role =
        revoke_roles ?ctx id (ActorRoleSet.singleton role)
      ;;

      let find_authorizable ?ctx (typ : 'kind) (id : Uuid.Actor.t) =
        let open Lwt_result.Infix in
        Authorizable.mem ?ctx id
        >>= fun exists ->
        if exists
        then find ?ctx typ id
        else
          Lwt_result.fail
            (Format.asprintf
               "Authorizable %s doesn't exist."
               (Uuid.Actor.to_string id))
      ;;

      (** This convenience function should be used to decorate the
          [to_authorizable] * functions of authorizable modules. The newly
          decorated function connects * to the persistent backend to ensure that
          the authorizable's roles and ownership * are consistent in both
          spaces. *)
      let decorate ?ctx (to_authorizable : 'a -> 'kind AuthorizableActor.t)
        : 'a -> ('kind AuthorizableActor.t, string) Lwt_result.t
        =
       fun x ->
        let open Lwt_result.Syntax in
        let ent = to_authorizable x in
        let uuid = ent.uuid in
        let* mem = Authorizable.mem ?ctx uuid in
        if mem
        then
          let* ent' = find_authorizable ?ctx ent.typ uuid in
          let roles = ActorRoleSet.union ent.roles ent'.roles in
          let* () = grant_roles ?ctx uuid roles in
          let* owner =
            match ent.owner, ent'.owner with
            | Some owner, None ->
              let* () = save_owner ?ctx ~owner uuid in
              Lwt.return_ok (Some owner)
            | None, Some owner -> Lwt.return_ok (Some owner)
            | None, None -> Lwt.return_ok None
            | Some x, Some y when x <> y ->
              (* Still unclear what the desirable behaviour is in this case. *)
              (* Lwt_result.fail( "decorate: both the database and the decorated
                 function \ returned distinct values for the owner of
                 authorizable " ^ Uuid.to_string uuid) *)
              let* () = save_owner ?ctx ~owner:x uuid in
              Lwt.return_ok (Some x)
            | Some x, Some _ (* when x = y *) -> Lwt.return_ok (Some x)
          in
          Lwt.return_ok AuthorizableActor.{ uuid; roles; owner; typ = ent.typ }
        else
          let* () = Authorizable.create ?ctx ?owner:ent.owner ent.roles uuid in
          Lwt.return_ok ent
     ;;

      let find_roles_exn ?ctx = with_exn find_roles ?ctx "find_roles_exn"
    end

    module Target = struct
      module AuthorizableActor = Authorizable
      include Target

      let flatten = CCFun.(CCResult.flatten_l %> CCResult.map CCList.flatten)

      let decorate ?ctx (to_authorizable : 'a -> 'kind AuthorizableTarget.t)
        : 'a -> ('kind AuthorizableTarget.t, string) Lwt_result.t
        =
       fun x ->
        let open Lwt_result.Syntax in
        let (ent : 'kind AuthorizableTarget.t) = to_authorizable x in
        let* mem = Authorizable.mem ?ctx ent.uuid in
        if mem
        then
          let* ent' = find ?ctx ent.typ ent.uuid in
          let* owner =
            match ent.owner, ent'.owner with
            | Some owner, None ->
              let* () = save_owner ?ctx ~owner ent.uuid in
              Lwt.return_ok (Some owner)
            | None, Some owner -> Lwt.return_ok (Some owner)
            | None, None -> Lwt.return_ok None
            | Some x, Some y when x <> y ->
              (* Still unclear what the desirable behaviour is in this case. *)
              (* Lwt_result.fail( "decorate: both the database and the decorated
                 function \ returned distinct values for the owner of
                 authorizable " ^ Uuid.to_string ent.uuid) *)
              let* () = save_owner ?ctx ~owner:x ent.uuid in
              Lwt.return_ok (Some x)
            | Some x, Some _ (* when x = y *) -> Lwt.return_ok (Some x)
          in
          AuthorizableTarget.make ?owner ent.entity ent.typ ent.uuid
          |> Lwt.return_ok
        else
          let* () =
            Authorizable.create ?ctx ?owner:ent.owner ent.entity ent.uuid
          in
          Lwt.return_ok ent
     ;;

      let find_checker ?ctx { AuthorizableTarget.uuid; owner; _ } =
        let open Lwt_result.Syntax in
        let* roles = find_roles ?ctx uuid in
        let%lwt auth_rules =
          TargetRoleSet.elements roles
          |> CCList.flat_map (fun m -> [ `TargetEntity m; `Target (m, uuid) ])
          |> Lwt_list.map_s (Rule.find_all ?ctx)
        in
        let* auth_rules = auth_rules |> flatten |> Lwt_result.lift in
        Lwt.return_ok
        @@ fun actor action ->
        let open Uuid in
        let is_owner =
          owner
          |> CCOption.map_or
               ~default:false
               (Actor.equal actor.AuthorizableActor.uuid)
        in
        let is_self =
          uuid
          |> Target.equal (actor.uuid |> Actor.to_string |> Target.of_string_exn)
        in
        if is_self || is_owner then true else exists_in auth_rules actor action
      ;;

      let find_role_checker ?ctx role_set =
        let open Lwt_result.Syntax in
        let%lwt auth_rules =
          TargetRoleSet.elements role_set
          |> CCList.map (fun r -> `TargetEntity r)
          |> Lwt_list.map_s (Rule.find_all ?ctx)
        in
        let* auth_rules = auth_rules |> flatten |> Lwt_result.lift in
        Lwt.return_ok @@ exists_in auth_rules
      ;;
    end

    (* Because of effects that look like [`Action A, `Target X] we need to make
       an extra pass to get all of entity X's roles, because if you have
       permission to do A to one of X's roles, then you should be able to do A
       to X. *)
    let rec expand_effects ?ctx (effects : Authorizer.effect list)
      : (Authorizer.effect list, string) result Lwt.t
      =
      let open CCFun in
      let open Lwt.Infix in
      let flatten =
        CCResult.flatten_l
        %> CCResult.map CCList.flatten
        %> CCResult.map (CCList.uniq ~eq:Authorizer.equal_effect)
      in
      let expand =
        Lwt_list.map_s (fun ((act, spec) as effect : Authorizer.effect) ->
          match spec with
          | `TargetEntity role | `Target (role, _) ->
            let fcn = Dependency.find role in
            fcn ?ctx spec
            >|= CCResult.to_opt
            >|= CCOption.flatten
            >>= fun inp ->
            (* let _ = failwith (Format.asprintf "%s" ([%show:
               Authorizer.Target.spec option] inp)) in *)
            (match inp with
             | Some parent_spec ->
               let parent_effect = act, parent_spec in
               expand_effects ?ctx [ parent_effect ]
               >|= CCResult.map (fun m -> effect :: parent_effect :: m)
             | None -> Lwt.return_ok [ effect ]))
      in
      effects
      |> expand
      >|= flatten
      |> Lwt_result.map_error
           (Format.asprintf
              "Failed to expand the effects of the target. Error message: %s")
    ;;

    (** [wrap_function ?error ~effects f] produces a wrapped version of [f]
        which checks permissions and gracefully reports authorization errors. *)
    let wrap_function
      ?ctx
      (error : string -> 'etyp)
      (effects : Authorizer.effect list)
      (fcn : 'param -> ('rval, 'etyp) Lwt_result.t)
      =
      let open Lwt_result.Infix in
      let open Lwt_result.Syntax in
      let* (collect_cans : ('a authorizable -> (unit, 'etyp) result) list) =
        effects
        |> Lwt_list.map_s (fun (action, target) ->
             (match target with
              | `Target (_, uuid) ->
                Target.(find ?ctx () uuid >>= find_checker ?ctx)
              | `TargetEntity role ->
                Target.find_role_checker ?ctx (TargetRoleSet.singleton role))
             |> Lwt_result.map (fun can actor ->
                  CCFun.(
                    can actor action
                    |> function
                    | true -> Ok ()
                    | false ->
                      Error
                        (Format.asprintf
                           "Entity %s does not have permission to %s target %s."
                           (Authorizable.to_string actor)
                           (Action.to_string action)
                           ([%show: Authorizer.Target.spec] target)
                        |> error))))
        |> Lwt.map CCResult.flatten_l
      in
      let can actor =
        CCList.map (fun can -> can actor) collect_cans
        |> CCResult.flatten_l
        |> CCResult.map (fun (_ : unit list) -> ())
        |> Lwt_result.lift
      in
      Lwt.return_ok (fun actor param ->
        let* () = can actor in
        fcn param)
    ;;

    module Rule = struct
      include Rule

      (** [save_rules rules] adds all the permissions [rules] to the backend. If
          there is an error at any point, it returns a `result` containing all
          of the items that were not added. *)
      let save_all ?ctx =
        Lwt_list.fold_left_s
          (fun acc x ->
            match%lwt Rule.save ?ctx x with
            | Ok () -> CCResult.map (CCList.cons x) acc |> Lwt_result.lift
            | Error _ -> CCResult.map_err (CCList.cons x) acc |> Lwt_result.lift)
          (Ok [])
      ;;

      (** [collect_rules e] Query the database for a list of rules pertaining to
          the effects [e]. *)
      let collect_rules ?ctx (effects : Authorizer.effect list) =
        let open CCFun in
        let flatten = CCResult.flatten_l %> CCResult.map CCList.flatten in
        Lwt_result.map_error
          (Format.asprintf
             "Failed to collect rules for effects list %s. Error message: %s"
             ([%show: Authorizer.effect list] effects))
        @@
        let open Lwt_result.Infix in
        let open Authorizer.Auth_rule_set in
        effects
        |> expand_effects ?ctx
        >>= Lwt_list.map_s (fun (action, target) ->
              Rule.find_all ?ctx target
              >|= CCList.filter (fun (_, rule_action, _) ->
                    action = rule_action || rule_action = `Manage))
            %> Lwt.map flatten
        >|= flip (CCList.fold_right add) empty
        >|= elements
      ;;

      let find_all_exn ?ctx = with_exn Rule.find_all ?ctx "find_all_exn"
      let save_exn ?ctx = with_exn Rule.save ?ctx "save_exn"
      let delete_exn ?ctx = with_exn Rule.delete ?ctx "delete_exn"
    end

    let checker_of_effects ?ctx effects actor : (unit, string) result Lwt.t =
      let ( >>> ) = Lwt_result.bind_result in
      Rule.collect_rules ?ctx effects
      >>> Authorizer.checker_of_rules ~any_of:true actor
    ;;
  end
end
