open CCFun

let src = Logs.Src.create "guardian"

type context = (string * string) list

module type RoleSig = Role.Sig

module Uuid = Uuid
module Action = Action

module Utils = struct
  let hide_typ f _ = Format.pp_print_string f ""

  (** turn a single argument function returning a [result] into one that raises
      a [Failure] instead *)
  let with_exn ?ctx f name arg =
    match%lwt f ?ctx arg with
    | Ok x -> Lwt.return x
    | Error s -> failwith @@ Format.asprintf "%s failed: %s" name s
  ;;

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

module Make (ActorRoles : RoleSig) (TargetRoles : RoleSig) = struct
  module ParentTyp = TargetRoles

  module RoleSet : Role_set.S with type elt = ActorRoles.t =
    Role_set.Make (ActorRoles)

  module ActorSpec = Spec.Make (ActorRoles) (Uuid.Actor)
  module TargetSpec = Spec.Make (TargetRoles) (Uuid.Target)

  module Rule = struct
    module Core = struct
      type t = ActorSpec.t * Action.t * TargetSpec.t
      [@@deriving eq, show, ord, yojson]
    end

    module Set = Set.Make (Core)
    include Core
  end

  module Effect = struct
    (** [action, target] Denotes an effect a function may have on and therefore
        which permissions an actor needs to invoke it. *)

    type t = Action.t * TargetSpec.t [@@deriving eq, show, ord, yojson]

    let create action target_spec = action, target_spec

    let is_valid ((t_action, t_spec) : t) ((action, spec) : t) =
      Action.is_valid ~matches:t_action action
      && TargetSpec.is_valid t_spec spec
    ;;
  end

  module EffectSet = struct
    type t =
      | And of t list
      | Or of t list
      | One of Effect.t
    [@@deriving eq, show, ord, yojson]

    let and_ m = And m
    let or_ m = Or m
    let one m = One m
  end [@warning "-4"]

  module Actor = struct
    type 'a t =
      { uuid : Uuid.Actor.t
      ; owner : Uuid.Actor.t option
      ; roles : RoleSet.t
      ; typ : 'a
      }
    [@@deriving eq, ord, show, yojson]

    let show t = show Utils.hide_typ t
    let pp t = pp Utils.hide_typ t
    let make ?owner roles typ uuid = { uuid; owner; roles; typ }

    let a_owns_b a b =
      CCOption.map_or ~default:false (fun b' -> a.uuid = b') b.owner
    ;;

    let has_role { roles; _ } = flip RoleSet.mem roles
  end

  module type ActorSig = sig
    type t

    (** [to_authorizable x] converts [x] to a uniquely identifiable object,
        complete * with roles. The [authorizable] may not, however, be converted
        back into type [t]. **)
    val to_authorizable
      :  ?ctx:context
      -> t
      -> (ActorRoles.t Actor.t, string) Lwt_result.t
  end

  module Target = struct
    type 'a t =
      { uuid : Uuid.Target.t
      ; owner : Uuid.Actor.t option
      ; typ : 'a
      }
    [@@deriving eq, ord, show, yojson]

    let show t = show Utils.hide_typ t
    let pp t = pp Utils.hide_typ t
    let make ?owner typ uuid = { uuid; owner; typ }
  end

  module type TargetSig = sig
    type t

    (** [to_authorizable x] converts [x] to a uniquely identifiable object,
        complete * with roles. The [authorizable] may not, however, be converted
        back into type [t]. **)
    val to_authorizable
      :  ?ctx:context
      -> t
      -> (TargetRoles.t Target.t, string) Lwt_result.t
  end

  module Dependency = struct
    module Key = struct
      type t = TargetRoles.t * TargetRoles.t [@@deriving eq, ord, show]
    end

    module Map = CCMap.Make (Key)

    type parent =
      ?ctx:context -> Effect.t -> (Effect.t option, string) Lwt_result.t

    let registered : parent Map.t ref = ref Map.empty

    let register
      ?(tags : Logs.Tag.set option)
      ?(ignore_duplicates = false)
      typ
      parent_kind
      (parent_fcn : parent)
      =
      let key = typ, parent_kind in
      let found = Map.find_opt key !registered in
      let msg =
        [%show: Key.t] %> Format.asprintf "Found duplicate registration: %s"
      in
      match found, ignore_duplicates with
      | None, _ ->
        registered := Map.add key parent_fcn !registered;
        Ok ()
      | Some _, true ->
        Logs.debug (fun m -> m ?tags "%s" (msg key));
        Ok ()
      | Some _, false -> Error (msg key)
    ;;

    let register_all dependencies =
      let open CCResult in
      dependencies
      |> CCList.map (fun (typ, parent, fcn) -> register typ parent fcn)
      |> flatten_l
      >|= fun (_ : unit list) -> ()
    ;;

    let find_opt (typ : TargetRoles.t) (parent_kind : TargetRoles.t)
      : parent option
      =
      Map.find_opt (typ, parent_kind) !registered
    ;;

    let find
      ?(default_fcn = fun ?ctx:_ _ -> Lwt_result.return None)
      (typ : TargetRoles.t)
      (parent_kind : TargetRoles.t)
      : parent
      =
      find_opt typ parent_kind
      |> function
      | Some parent_fcn -> parent_fcn
      | None -> default_fcn
    ;;

    let find_all (typ : TargetRoles.t) : parent list =
      Map.filter (fun (kind, _) _ -> TargetRoles.equal kind typ) !registered
      |> Map.to_list
      |> CCList.map snd
    ;;

    let find_all_combined (typ : TargetRoles.t)
      : ?ctx:context -> Effect.t -> (Effect.t list, string) Lwt_result.t
      =
     fun ?ctx effect ->
      let open Lwt.Infix in
      find_all typ
      |> Lwt_list.map_s (fun fcn ->
           fcn ?ctx effect |> Lwt_result.map CCOption.to_list)
      >|= CCResult.(flatten_l %> map CCList.flatten)
   ;;
  end

  module Authorizer = struct
    let check_effect ?(tags : Logs.Tag.set option) all_rules actor effect =
      let is_matched = function
        | ActorSpec.Id (role, uuid) ->
          uuid = actor.Actor.uuid && RoleSet.mem role actor.Actor.roles
        | ActorSpec.Entity role -> RoleSet.mem role actor.Actor.roles
      in
      let rule =
        CCList.filter
          (fun ((actor', action, target) : Rule.t) ->
            Effect.create action target |> Effect.is_valid effect
            && is_matched actor')
          all_rules
      in
      if CCList.is_empty rule |> not
      then Ok ()
      else (
        let msg =
          Format.asprintf
            "Actor %s does not have permission to %s"
            ([%show: Actor.t] actor)
            ([%show: Effect.t] effect)
        in
        Logs.info ~src (fun m -> m ?tags "%s" msg);
        Error msg)
    ;;

    let actor_in_rule actor ((actor', _, _) : Rule.t) =
      match actor' with
      | ActorSpec.Id (role, uuid) ->
        uuid = actor.Actor.uuid && RoleSet.mem role actor.Actor.roles
      | ActorSpec.Entity role -> RoleSet.mem role actor.Actor.roles
    ;;

    let actor_in_rule_res actor (rule : Rule.t) =
      let open CCResult in
      if actor_in_rule actor rule
      then Ok ()
      else (
        let msg =
          Format.asprintf
            "Actor %s does not have permission to %s"
            ([%show: Actor.t] actor)
            ([%show: Rule.t] rule)
        in
        Logs.info ~src (fun m -> m "%s" msg);
        Error msg)
    ;;

    (** Convenience function to return a [can] function. Takes an optional
        target specification (for error reporting purposes) and a list of
        [guardian] rules of the form [actor, action, target] and returns a
        function that looks like:

        [val can : actor:\[ whatever \] Guard.Actor.t -> (unit, string) result]

        [any_of]: indicates that the checker should pass if any of the rules in
        the list is satisfied. The default behaviour is to only pass if all
        rules are. *)
    let can_for_rules ?(any_of = false) (rules : Rule.t list) actor =
      let open CCResult in
      let results = rules |> CCList.map (actor_in_rule_res actor) in
      match any_of with
      | true when CCList.exists (( = ) (Ok ())) results -> Ok ()
      | true ->
        Error
          (Format.asprintf
             "Actor %s does not satisfy any of the following rules: %s"
             ([%show: Actor.t] actor)
             ([%show: Rule.t list] rules))
      | false ->
        results
        |> CCResult.flatten_l
        |> CCResult.map (fun (_ : unit list) -> ())
    ;;
  end

  module type Persistence_s =
    Persistence.Contract
      with type 'a actor = 'a Actor.t
       and type 'b target = 'b Target.t
       and type actor_spec = ActorSpec.t
       and type effect = Effect.t
       and type effect_set = EffectSet.t
       and type kind = TargetRoles.t
       and type parent_kind = ParentTyp.t
       and type role_set = RoleSet.t
       and type roles = ActorRoles.t
       and type rule = Rule.t
       and type target_spec = TargetSpec.t

  module Make_persistence
    (Backend : Persistence.Backend
                 with type 'a actor = 'a Actor.t
                  and type 'b target = 'b Target.t
                  and type actor_spec = ActorSpec.t
                  and type effect = Effect.t
                  and type effect_set = EffectSet.t
                  and type kind = TargetRoles.t
                  and type parent_kind = ParentTyp.t
                  and type role_set = RoleSet.t
                  and type roles = ActorRoles.t
                  and type rule = Rule.t
                  and type target_spec = TargetSpec.t) : Persistence_s = struct
    include Backend
    module Dependency = Dependency

    module Utils = struct
      include Utils

      let exists_in (rules : rule list) actor action =
        CCList.exists
          (fun (actor', action', _) ->
            match actor' with
            | ActorSpec.Id (role, id) ->
              actor.Actor.uuid = id
              && Action.is_valid ~matches:action action'
              && RoleSet.mem role actor.Actor.roles
            | ActorSpec.Entity role ->
              RoleSet.mem role actor.Actor.roles
              && Action.is_valid ~matches:action action')
          rules
      ;;
    end

    module Rule = struct
      include Repo.Rule

      (** [save_all rules] adds all the permissions [rules] to the backend. If
          there is an error at any point, it returns a `result` containing all
          of the items that were not added. *)
      let save_all ?ctx =
        Lwt_list.fold_left_s
          (fun acc x ->
            match%lwt save ?ctx x with
            | Ok () -> CCResult.map (CCList.cons x) acc |> Lwt_result.lift
            | Error _ -> CCResult.map_err (CCList.cons x) acc |> Lwt_result.lift)
          (Ok [])
      ;;

      let delete_exn ?ctx = Utils.with_exn delete ?ctx "delete_exn"
    end

    module Actor = struct
      include Actor
      include Repo.Actor

      let revoke_role ?ctx id role =
        revoke_roles ?ctx id (RoleSet.singleton role)
      ;;

      let find_authorizable ?ctx (typ : 'kind) (id : Uuid.Actor.t) =
        let open Lwt_result.Infix in
        mem ?ctx id
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
      let decorate ?ctx (to_authorizable : 'a -> 'kind actor)
        : 'a -> ('kind actor, string) Lwt_result.t
        =
       fun x ->
        let open Lwt_result.Syntax in
        let ({ Actor.uuid; owner; roles; typ } as entity : 'kind actor) =
          to_authorizable x
        in
        let* mem = mem ?ctx uuid in
        if mem
        then
          let* entity' = find_authorizable ?ctx typ uuid in
          let roles = RoleSet.union roles entity'.Actor.roles in
          let* () = grant_roles ?ctx uuid roles in
          let* owner =
            match owner, entity'.Actor.owner with
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
          Lwt.return_ok { Actor.uuid; roles; owner; typ }
        else
          let* () = create ?ctx ?owner roles uuid in
          Lwt.return_ok entity
     ;;

      let find_roles_exn ?ctx = Utils.with_exn find_roles ?ctx "find_roles_exn"
    end

    module Target = struct
      include Target
      include Repo.Target

      let decorate ?ctx (to_authorizable : 'a -> 'kind target)
        : 'a -> ('kind target, string) Lwt_result.t
        =
       fun x ->
        let open Lwt_result.Syntax in
        let ({ Target.uuid; owner; typ } as entity : 'kind target) =
          to_authorizable x
        in
        let* mem = mem ?ctx uuid in
        if mem
        then
          let* entity' = find ?ctx typ uuid in
          let* owner =
            match owner, entity'.Target.owner with
            | Some owner, None ->
              let* () = save_owner ?ctx ~owner uuid in
              Lwt.return_ok (Some owner)
            | None, Some owner -> Lwt.return_ok (Some owner)
            | None, None -> Lwt.return_ok None
            | Some x, Some y when x <> y ->
              (* Still unclear what the desirable behaviour is in this case. *)
              (* Lwt_result.fail( "decorate: both the database and the decorated
                 function \ returned distinct values for the owner of
                 authorizable " ^ Uuid.to_string entity.uuid) *)
              let* () = save_owner ?ctx ~owner:x uuid in
              Lwt.return_ok (Some x)
            | Some x, Some _ (* when x = y *) -> Lwt.return_ok (Some x)
          in
          Target.make ?owner typ uuid |> Lwt.return_ok
        else
          let* () = create ?ctx ?owner typ uuid in
          Lwt.return_ok entity
     ;;

      let find_checker ?ctx { Target.uuid; owner; _ } =
        let open Lwt_result.Syntax in
        let* kind = find_kind ?ctx uuid in
        let%lwt rules =
          TargetSpec.[ Entity kind; Id (kind, uuid) ]
          |> Lwt_list.map_s (Rule.find_all ?ctx)
          |> Lwt.map CCList.flatten
        in
        Lwt.return_ok
        @@ fun actor action ->
        let is_owner =
          owner
          |> CCOption.map_or ~default:false (Uuid.Actor.equal actor.Actor.uuid)
        in
        let is_self target_id { Actor.uuid; _ } =
          let open Uuid in
          target_id
          |> Target.equal (uuid |> Actor.to_string |> Target.of_string_exn)
        in
        if is_self uuid actor || is_owner
        then true
        else Utils.exists_in rules actor action
      ;;

      let find_typ_checker ?ctx typ =
        let%lwt rules = Rule.find_all ?ctx (TargetSpec.Entity typ) in
        Lwt.return_ok @@ Utils.exists_in rules
      ;;
    end

    (* For transitional effects, uses the registered dependencies to look for
       the parent object. *)
    let expand_set ?ctx (effects : EffectSet.t)
      : (EffectSet.t, string) result Lwt.t
      =
      let open Lwt_result.Infix in
      let rec expand (effects : EffectSet.t)
        : (EffectSet.t, string) Lwt_result.t
        =
        let open EffectSet in
        let find_parent entity spec : (effect list, string) Lwt_result.t =
          let fcn = Dependency.find_all_combined entity in
          fcn ?ctx spec
        in
        let find_all = Lwt_list.map_s expand %> Lwt.map CCResult.flatten_l in
        match effects with
        | One effect ->
          (match snd effect with
           | TargetSpec.Entity entity | TargetSpec.Id (entity, _) ->
             find_parent entity effect
             >>= (function
             | [] -> One effect |> Lwt.return_ok
             | parent_effects ->
               CCList.map one parent_effects
               |> or_ %> expand
               >|= fun parents -> Or [ One effect; parents ]))
        | Or effects -> effects |> find_all >|= or_
        | And effects -> effects |> find_all >|= and_
      in
      effects
      |> expand
      |> Lwt_result.map_error
           (Format.asprintf
              "Failed to expand the effects of the target. Error message: %s")
    ;;

    let validate_set
      ?ctx
      (error : string -> 'etyp)
      (to_match : EffectSet.t)
      (actor : 'a actor)
      : (unit, 'etyp) Lwt_result.t
      =
      let open Lwt_result.Infix in
      let ( >>> ) = Lwt_result.bind_result in
      let map_error = Lwt_result.map_error error in
      let rec find_checker =
        let open EffectSet in
        let find ((action, spec) : Action.t * TargetSpec.t) =
          (match spec with
           | TargetSpec.Id (typ, uuid) ->
             Target.(find ?ctx typ uuid >>= find_checker ?ctx)
           | TargetSpec.Entity role -> Target.find_typ_checker ?ctx role)
          >|= fun checker_fcn -> checker_fcn actor action
        in
        function
        | One effect -> find effect
        | Or (rule :: rules) ->
          let%lwt init = find_checker rule in
          Lwt_list.fold_left_s
            (fun ini rule ->
              match ini with
              | Ok true -> Lwt.return_ok true
              | Ok false -> find_checker rule
              | Error err -> Lwt.return_error err)
            init
            rules
        | And (rule :: rules) ->
          let%lwt init = find_checker rule in
          Lwt_list.fold_left_s
            (fun ini rule ->
              match ini with
              | Ok true -> find_checker rule
              | Ok false -> Lwt.return_ok false
              | Error err -> Lwt.return_error err)
            init
            rules
        | Or [] | And [] -> Lwt.return_error "Empty Authentication Set"
      in
      let validate = function
        | true -> Ok ()
        | false ->
          Error
            (Format.asprintf
               "Entity %s: Permission denied for %s"
               ([%show: Actor.t] actor)
               ([%show: EffectSet.t] to_match))
      in
      to_match |> expand_set ?ctx >>= find_checker >>> validate |> map_error
    ;;

    (** [wrap_function ?error ~effect f] produces a wrapped version of [f] which
        checks permissions and gracefully reports authorization errors. *)
    let wrap_function
      ?ctx
      (error : string -> 'etyp)
      (to_match : EffectSet.t)
      (fcn : 'param -> ('rval, 'etyp) Lwt_result.t)
      =
      let open Lwt_result.Syntax in
      let can = validate_set ?ctx error to_match in
      Lwt.return_ok (fun actor param ->
        let* () = can actor in
        fcn param)
    ;;

    let validate_effects ?ctx (set : EffectSet.t) actor
      : (unit, string) result Lwt.t
      =
      validate_set ?ctx CCFun.id set actor
    ;;
  end
end
