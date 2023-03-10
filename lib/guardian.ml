open CCFun

let src = Logs.Src.create "guardian"

type context = (string * string) list

module Uuid = Uuid

module type RoleSig = Role.Sig

module Utils = struct
  let hide_typ f _ = Format.pp_print_string f ""

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
  module ParentTyp = T

  module ActorSpec = struct
    type t =
      [ `ActorEntity of A.t
      | `Actor of A.t * Uuid.Actor.t
      ]
    [@@deriving eq, show, ord, yojson]

    let is_valid (target : t) (spec : t) =
      match target, spec with
      | `ActorEntity t_entity, `ActorEntity s_entity
      | `Actor (t_entity, _), `ActorEntity s_entity -> A.equal t_entity s_entity
      | `ActorEntity _, `Actor (_, _) -> false
      | `Actor (t_entity, t_uuid), `Actor (s_entity, s_uuid) ->
        A.equal t_entity s_entity && Uuid.Actor.equal t_uuid s_uuid
    ;;
  end

  module TargetSpec = struct
    type t =
      [ `TargetEntity of T.t
      | `Target of T.t * Uuid.Target.t
      ]
    [@@deriving eq, show, ord, yojson]

    let is_valid (target : t) (spec : t) =
      match target, spec with
      | `TargetEntity t_entity, `TargetEntity s_entity
      | `Target (t_entity, _), `TargetEntity s_entity ->
        T.equal t_entity s_entity
      | `TargetEntity _, `Target (_, _) -> false
      | `Target (t_entity, t_uuid), `Target (s_entity, s_uuid) ->
        T.equal t_entity s_entity && Uuid.Target.equal t_uuid s_uuid
    ;;
  end

  module Rule = struct
    type t = ActorSpec.t * Action.t * TargetSpec.t
    [@@deriving eq, show, ord, yojson]
  end

  module RuleSet = Set.Make (Rule)

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

  module AuthenticationSet = struct
    type t =
      | And of t list
      | Or of t list
      | One of Effect.t
    [@@deriving eq, show, ord, yojson]

    let and_ m = And m
    let or_ m = Or m
    let one m = One m
  end [@warning "-4"]

  module AuthRuleSet = struct
    type t =
      | And of t list
      | Or of t list
      | One of Rule.t
    [@@deriving eq, show, ord, yojson]

    let and_ m = And m
    let or_ m = Or m
    let one m = One m
  end [@warning "-4"]

  module Dependency = struct
    module Key = struct
      type t = T.t * T.t [@@deriving eq, ord, show]
    end

    module Map = CCMap.Make (Key)

    type parent =
      ?ctx:context -> Effect.t -> (Effect.t option, string) Lwt_result.t

    let registered : parent Map.t ref = ref Map.empty

    let register
      ?(tags : Logs.Tag.set option)
      ?(ignore_duplicates = false)
      typ
      parent_typ
      (parent_fcn : parent)
      =
      let key = typ, parent_typ in
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

    let find_opt (typ : T.t) (parent_typ : T.t) : parent option =
      Map.find_opt (typ, parent_typ) !registered
    ;;

    let find
      ?(default_fcn = fun ?ctx:_ _ -> Lwt_result.return None)
      (typ : T.t)
      (parent_typ : T.t)
      : parent
      =
      find_opt typ parent_typ
      |> function
      | Some parent_fcn -> parent_fcn
      | None -> default_fcn
    ;;

    let find_all (typ : T.t) : parent list =
      Map.filter (fun (kind, _) _ -> T.equal kind typ) !registered
      |> Map.to_list
      |> CCList.map snd
    ;;

    let find_all_combined (typ : T.t)
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

  module Authorizable = struct
    type 'a t =
      { uuid : Uuid.Actor.t
      ; owner : Uuid.Actor.t option
      ; roles : ActorRoleSet.t
      ; typ : 'a
      }
    [@@deriving eq, ord, show, yojson]

    let show t = show Utils.hide_typ t
    let pp t = pp Utils.hide_typ t
    let make ?owner roles typ uuid = { uuid; owner; roles; typ }

    let a_owns_b a b =
      CCOption.map_or ~default:false (fun b' -> a.uuid = b') b.owner
    ;;

    let has_role { roles; _ } = flip ActorRoleSet.mem roles
  end

  module AuthorizableTarget = struct
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

  module Authorizer = struct
    let check_effect ?(tags : Logs.Tag.set option) all_rules actor effect =
      let is_matched = function
        | `Actor (role, uuid) ->
          uuid = actor.Authorizable.uuid
          && ActorRoleSet.mem role actor.Authorizable.roles
        | `ActorEntity role -> ActorRoleSet.mem role actor.Authorizable.roles
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
            ([%show: Authorizable.t] actor)
            ([%show: Effect.t] effect)
        in
        Logs.info ~src (fun m -> m ?tags "%s" msg);
        Error msg)
    ;;

    let actor_in_rule actor ((actor', _, _) : Rule.t) =
      match actor' with
      | `Actor (role, uuid) ->
        uuid = actor.Authorizable.uuid
        && ActorRoleSet.mem role actor.Authorizable.roles
      | `ActorEntity role -> ActorRoleSet.mem role actor.Authorizable.roles
    ;;

    let actor_in_rule_res actor (rule : Rule.t) =
      let open CCResult in
      if actor_in_rule actor rule
      then Ok ()
      else (
        let msg =
          Format.asprintf
            "Actor %s does not have permission to %s"
            ([%show: Authorizable.t] actor)
            ([%show: Rule.t] rule)
        in
        Logs.info ~src (fun m -> m "%s" msg);
        Error msg)
    ;;

    (** Convenience function to return a [can] function. Takes an optional
        target specification (for error reporting purposes) and a list of
        [guardian] rules of the form [actor, action, target] and returns a
        function that looks like:

        [any_of]: indicates that the checker should pass if any of the rules in
        the list is satisfied. The default behaviour is to only pass if all
        rules are.
        [val can : actor:\[ whatever \] Guard.Authorizable.t -> (unit, string) result] *)
    let actor_in_rules ?(any_of = false) actor (rules : Rule.t list) =
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
             else (
               let msg =
                 Format.asprintf
                   "Actor %s does not have permission to %s"
                   ([%show: Authorizable.t] actor)
                   ([%show: Effect.t] (Effect.create action target))
               in
               Logs.info ~src (fun m -> m "%s" msg);
               Error msg))
      in
      match any_of with
      | true when CCList.exists (( = ) (Ok ())) results -> Ok ()
      | true ->
        Error
          (Format.asprintf
             "Actor %s does not satisfy any of the following rules: %s"
             ([%show: Authorizable.t] actor)
             ([%show: Rule.t list] rules))
      | false ->
        results
        |> CCResult.flatten_l
        |> CCResult.map (fun (_ : unit list) -> ())
    ;;

    let validate_rule_set actor set =
      let check =
        actor_in_rule_res actor
        %> CCResult.map_err (fun err ->
             Logs.debug (fun m -> m "%s" err);
             err)
      in
      let rec validate =
        let open AuthRuleSet in
        function
        | One rule -> check rule
        | Or (rule :: rules) ->
          CCList.fold_left
            CCResult.(
              fun ini rule ->
                if CCResult.is_ok ini
                then Ok ()
                else ini >>= fun () -> validate rule)
            (validate rule)
            rules
        | And (rule :: rules) ->
          CCList.fold_left
            CCResult.(
              fun ini rule ->
                if CCResult.is_ok ini
                then ini >>= fun () -> validate rule
                else ini)
            (validate rule)
            rules
        | Or [] | And [] -> Error "No rules to validate."
      in
      validate set
    ;;

    module type ActorSig = sig
      type t

      (** [to_authorizable x] converts [x] to a uniquely identifiable object,
          complete * with roles. The [authorizable] may not, however, be
          converted back into type [t]. **)
      val to_authorizable
        :  ?ctx:context
        -> t
        -> (A.t Authorizable.t, string) Lwt_result.t
    end

    module type TargetSig = sig
      type t

      (** [to_authorizable x] converts [x] to a uniquely identifiable object,
          complete * with roles. The [authorizable] may not, however, be
          converted back into type [t]. **)
      val to_authorizable
        :  ?ctx:context
        -> t
        -> (T.t AuthorizableTarget.t, string) Lwt_result.t
    end
  end

  module type Persistence_s =
    Persistence.Contract
      with type 'a authorizable = 'a Authorizable.t
       and type 'b authorizable_target = 'b AuthorizableTarget.t
       and type actor_role_set = ActorRoleSet.t
       and type actor_spec = ActorSpec.t
       and type auth_rule = Rule.t
       and type auth_rule_set = AuthRuleSet.t
       and type effect = Effect.t
       and type auth_set = AuthenticationSet.t
       and type target_spec = TargetSpec.t
       and type target_typ = T.t
       and type parent_typ = ParentTyp.t
       and type role = A.t

  module Make_persistence
    (Backend : Persistence.Backend
                 with type 'a authorizable = 'a Authorizable.t
                  and type 'b authorizable_target = 'b AuthorizableTarget.t
                  and type actor_role_set = ActorRoleSet.t
                  and type actor_spec = ActorSpec.t
                  and type auth_rule = Rule.t
                  and type auth_rule_set = AuthRuleSet.t
                  and type effect = Effect.t
                  and type auth_set = AuthenticationSet.t
                  and type target_spec = TargetSpec.t
                  and type target_typ = T.t
                  and type parent_typ = ParentTyp.t
                  and type role = A.t) : Persistence_s = struct
    include Backend
    module Dependency = Dependency
    module Action = Action

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
            && Action.is_valid ~matches:action action'
            && ActorRoleSet.mem role actor.Authorizable.roles
          | `ActorEntity role ->
            ActorRoleSet.mem role actor.Authorizable.roles
            && Action.is_valid ~matches:action action')
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
        let ({ AuthorizableActor.uuid; owner; roles; typ } as entity
              : 'kind authorizable)
          =
          to_authorizable x
        in
        let* mem = Authorizable.mem ?ctx uuid in
        if mem
        then
          let* entity' = find_authorizable ?ctx typ uuid in
          let roles =
            ActorRoleSet.union roles entity'.AuthorizableActor.roles
          in
          let* () = grant_roles ?ctx uuid roles in
          let* owner =
            match owner, entity'.AuthorizableActor.owner with
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
          Lwt.return_ok AuthorizableActor.{ uuid; roles; owner; typ }
        else
          let* () = Authorizable.create ?ctx ?owner roles uuid in
          Lwt.return_ok entity
     ;;

      let find_roles_exn ?ctx = with_exn find_roles ?ctx "find_roles_exn"
    end

    module Target = struct
      module AuthorizableActor = Authorizable
      include Target

      let decorate ?ctx (to_authorizable : 'a -> 'kind AuthorizableTarget.t)
        : 'a -> ('kind AuthorizableTarget.t, string) Lwt_result.t
        =
       fun x ->
        let open Lwt_result.Syntax in
        let ({ AuthorizableTarget.uuid; owner; typ } as entity
              : 'kind AuthorizableTarget.t)
          =
          to_authorizable x
        in
        let* mem = Authorizable.mem ?ctx uuid in
        if mem
        then
          let* entity' = find ?ctx typ uuid in
          let* owner =
            match owner, entity'.AuthorizableTarget.owner with
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
          AuthorizableTarget.make ?owner typ uuid |> Lwt.return_ok
        else
          let* () = Authorizable.create ?ctx ?owner typ uuid in
          Lwt.return_ok entity
     ;;

      let find_checker ?ctx { AuthorizableTarget.uuid; owner; _ } =
        let open Lwt_result.Syntax in
        let* kind = find_kind ?ctx uuid in
        let%lwt auth_rules =
          [ `TargetEntity kind; `Target (kind, uuid) ]
          |> Lwt_list.map_s (Rule.find_all ?ctx)
          |> Lwt.map CCList.flatten
        in
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
          |> Target.equal
               (actor.AuthorizableActor.uuid
                |> Actor.to_string
                |> Target.of_string_exn)
        in
        if is_self || is_owner then true else exists_in auth_rules actor action
      ;;

      let find_typ_checker ?ctx typ =
        let%lwt auth_rules = Rule.find_all ?ctx (`TargetEntity typ) in
        Lwt.return_ok @@ exists_in auth_rules
      ;;
    end

    (* For transitional effects, uses the registered dependencies to look for
       the parent object. *)
    let expand_set ?ctx (effects : AuthenticationSet.t)
      : (AuthenticationSet.t, string) result Lwt.t
      =
      let open Lwt_result.Infix in
      let rec expand (effects : AuthenticationSet.t)
        : (AuthenticationSet.t, string) Lwt_result.t
        =
        let open AuthenticationSet in
        let find_parent entity spec : (effect list, string) Lwt_result.t =
          let fcn = Dependency.find_all_combined entity in
          fcn ?ctx spec
        in
        let find_all = Lwt_list.map_s expand %> Lwt.map CCResult.flatten_l in
        match effects with
        | One effect ->
          (match snd effect with
           | `TargetEntity entity | `Target (entity, _) ->
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
      (* let collect_rules ?ctx (effects : AuthenticationSet.t) =
         Lwt_result.map_error (Format.asprintf "Failed to collect rules for
         effects list %s. Error message: %s" ([%show: AuthenticationSet.t]
         effects)) @@ let open Lwt.Infix in let rules_of_effect (action, spec) :
         AuthRuleSet.t Lwt.t = Rule.find_all ?ctx spec >|= CCList.filter (fun
         (_, rule_action, _) -> Action.is_valid ~matches:action rule_action) >|=
         CCList.map AuthRuleSet.one %> AuthRuleSet.or_ in let rec find_rules :
         AuthenticationSet.t -> AuthRuleSet.t Lwt.t = let open AuthenticationSet
         in let rules_of_effects effects = Lwt_list.map_s find_rules effects in
         function | One effect -> rules_of_effect effect | Or effects ->
         rules_of_effects effects >|= AuthRuleSet.or_ | And effects ->
         rules_of_effects effects >|= AuthRuleSet.and_ in effects |> expand_set
         ?ctx |> flip Lwt_result.bind_lwt find_rules ;; *)

      let save_exn ?ctx = with_exn Rule.save ?ctx "save_exn"
      let delete_exn ?ctx = with_exn Rule.delete ?ctx "delete_exn"
    end

    let validate_set
      ?ctx
      (error : string -> 'etyp)
      (to_match : AuthenticationSet.t)
      (actor : 'a authorizable)
      : (unit, 'etyp) Lwt_result.t
      =
      let open Lwt_result.Infix in
      let ( >>> ) = Lwt_result.bind_result in
      let map_error = Lwt_result.map_error error in
      let rec find_checker =
        let open AuthenticationSet in
        let find ((action, spec) : Action.t * TargetSpec.t) =
          (match spec with
           | `Target (typ, uuid) ->
             Target.(find ?ctx typ uuid >>= find_checker ?ctx)
           | `TargetEntity role -> Target.find_typ_checker ?ctx role)
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
               ([%show: Authorizable.t] actor)
               ([%show: AuthenticationSet.t] to_match))
      in
      to_match |> expand_set ?ctx >>= find_checker >>> validate |> map_error
    ;;

    (** [wrap_function ?error ~effect f] produces a wrapped version of [f] which
        checks permissions and gracefully reports authorization errors. *)
    let wrap_function
      ?ctx
      (error : string -> 'etyp)
      (to_match : AuthenticationSet.t)
      (fcn : 'param -> ('rval, 'etyp) Lwt_result.t)
      =
      let open Lwt_result.Syntax in
      let can = validate_set ?ctx error to_match in
      Lwt.return_ok (fun actor param ->
        let* () = can actor in
        fcn param)
    ;;

    let validate_effects ?ctx (set : AuthenticationSet.t) actor
      : (unit, string) result Lwt.t
      =
      validate_set ?ctx CCFun.id set actor
    ;;
  end
end
