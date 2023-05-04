open CCFun

let src = Logs.Src.create "guardian"

type context = Persistence.context

module type RoleSig = Role.Sig

module Make (ActorRoles : RoleSig) (TargetRoles : RoleSig) = struct
  module Uuid = Uuid
  module Action = Action
  module RoleSet = Role_set.Make (ActorRoles)
  module ActorSpec = Spec.Make (ActorRoles) (Uuid.Actor)
  module TargetSpec = Spec.Make (TargetRoles) (Uuid.Target)

  module Rule = struct
    module Core = struct
      type t = ActorSpec.t * Action.t * TargetSpec.t
      [@@deriving eq, show, ord, yojson]
    end

    module Set = CCSet.Make (Core)
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

  module ValidationSet = struct
    type t =
      | And of t list
      | Or of t list
      | One of Effect.t
      | SpecificRole of ActorRoles.t
      | NotRole of ActorRoles.t
    [@@deriving eq, show, ord, yojson]

    let and_ m = And m
    let or_ m = Or m
    let one m = One m
    let specific_role m = SpecificRole m
    let not_role m = NotRole m
    let empty = Or []
  end

  module Actor = struct
    type 'a t =
      { uuid : Uuid.Actor.t
      ; owner : Uuid.Actor.t option
      ; roles : RoleSet.t
      ; typ : 'a
      }
    [@@deriving eq, ord, show, yojson]

    let id { uuid; _ } = uuid
    let owner { owner; _ } = owner
    let roles { roles; _ } = roles
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

    let id { uuid; _ } = uuid
    let owner { owner; _ } = owner
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

  module Relation = struct
    module Query = struct
      type t = string [@@deriving eq, ord, show]

      let create m = m
      let value m = m
    end

    type t = TargetRoles.t * TargetRoles.t * Query.t option
    [@@deriving eq, ord, show]
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

    (** [can_for_rules ?any_of rules] Convenience function to return a [can]
        function. Takes an optional target specification (for error reporting
        purposes) and a list of [guardian] rules of the form
        [actor, action, target] and returns a function that looks like:

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

  module type PersistenceSig =
    Persistence.Contract
      with type 'a actor = 'a Actor.t
       and type 'b target = 'b Target.t
       and type actor_spec = ActorSpec.t
       and type effect = Effect.t
       and type kind = TargetRoles.t
       and type query = Relation.Query.t
       and type relation = Relation.t
       and type role_set = RoleSet.t
       and type roles = ActorRoles.t
       and type rule = Rule.t
       and type target_spec = TargetSpec.t
       and type validation_set = ValidationSet.t

  module MakePersistence
    (Backend : Persistence.Backend
                 with type 'a actor = 'a Actor.t
                  and type 'b target = 'b Target.t
                  and type actor_spec = ActorSpec.t
                  and type effect = Effect.t
                  and type kind = TargetRoles.t
                  and type query = Relation.Query.t
                  and type relation = Relation.t
                  and type role_set = RoleSet.t
                  and type roles = ActorRoles.t
                  and type rule = Rule.t
                  and type target_spec = TargetSpec.t
                  and type validation_set = ValidationSet.t) : PersistenceSig =
  struct
    include Backend

    module Relation = struct
      include Relation

      module Cache = struct
        open CCCache

        let equal_relation (c1, o1, t1) (c2, o2, t2) =
          let ctx = [%show: (string * string) list] in
          CCOption.equal (fun a b -> CCString.equal (ctx a) (ctx b)) c1 c2
          && TargetRoles.equal o1 o2
          && TargetRoles.equal t1 t2
        ;;

        let lru
          : ( context option * kind * kind
          , (Query.t option, string) Lwt_result.t ) t
          =
          lru ~eq:equal_relation 2048
        ;;
      end

      let add
        ?ctx
        ?(tags : Logs.Tag.set option)
        ?(ignore_duplicates = false)
        ?to_target
        ~target
        origin
        =
        let%lwt found = Repo.Relation.find_query ?ctx origin target in
        let msg =
          Format.asprintf
            "Found duplicate registration: (%s, %s)"
            ([%show: TargetRoles.t] origin)
            ([%show: TargetRoles.t] target)
        in
        match found, ignore_duplicates with
        | Error _, _ -> Repo.Relation.upsert ?ctx ?query:to_target origin target
        | Ok _, true ->
          Logs.debug (fun m -> m ?tags "%s" msg);
          Lwt.return_ok ()
        | Ok _, false -> Lwt.return_error msg
      ;;

      let add_multiple ?ctx ?tags ?ignore_duplicates dependencies =
        let open Lwt_result.Infix in
        dependencies
        |> Lwt_list.map_s (fun (origin, target, to_target) ->
             add ?ctx ?tags ?ignore_duplicates ~target ?to_target origin)
        |> Lwt.map CCResult.flatten_l
        >|= fun (_ : unit list) -> ()
      ;;

      let find_query ?(ctx : context option) ~target origin
        : (Query.t option, string) Lwt_result.t
        =
        let find' (context, origin, target) =
          Repo.Relation.find_query ?ctx:context origin target
        in
        CCCache.with_cache Cache.lru find' (ctx, origin, target)
      ;;

      let find ?ctx ?default ~target origin =
        let open Lwt_result.Infix in
        find_query ?ctx ~target origin
        >|= fun query -> origin, target, CCOption.choice [ query; default ]
      ;;

      let find_opt ?ctx ~target origin =
        let open Lwt.Infix in
        find_query ?ctx ~target origin
        >|= CCResult.map_or ~default:None (fun query ->
              Some (origin, target, query))
      ;;

      let find_rec = Repo.Relation.find_rec
      let find_effects_rec = Repo.Relation.find_effects_rec
    end

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

      (** [save_all ?ctx rules] adds all the permissions [rules] to the backend.
          If there is an error at any point, it returns a `result` containing
          all of the items that were not added. *)
      let save_all ?ctx =
        Lwt_list.fold_left_s
          (fun acc x ->
            match%lwt save ?ctx x with
            | Ok () -> CCResult.map (CCList.cons x) acc |> Lwt_result.lift
            | Error (_ : string) ->
              CCResult.map_err (CCList.cons x) acc |> Lwt_result.lift)
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

      let find ?ctx (typ : 'kind) (id : Uuid.Actor.t) =
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

      (** [decorate ?ctx to_actor] This convenience function should be used to
          decorate the [actor] * functions of authorizable modules. The newly
          decorated function connects * to the persistent backend to ensure that
          the authorizable's roles and ownership * are consistent in both
          spaces. *)
      let decorate ?ctx (to_actor : 'a -> 'kind actor)
        : 'a -> ('kind actor, string) Lwt_result.t
        =
       fun x ->
        let open Lwt_result.Syntax in
        let ({ Actor.uuid; owner; roles; typ } as entity : 'kind actor) =
          to_actor x
        in
        let* mem = mem ?ctx uuid in
        if mem
        then
          let* entity' = find ?ctx typ uuid in
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

      (** [decorate ?ctx to_target] This convenience function should be used to
          decorate the [target] * functions of authorizable modules. The newly
          decorated function connects * to the persistent backend to ensure that
          the authorizable's roles and ownership * are consistent in both
          spaces. *)
      let decorate ?ctx (to_target : 'a -> 'kind target)
        : 'a -> ('kind target, string) Lwt_result.t
        =
       fun x ->
        let open Lwt_result.Syntax in
        let ({ Target.uuid; owner; typ } as entity : 'kind target) =
          to_target x
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

      (** [find_checker] find checker function for a specific target id *)
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

      (** [find_kind_checker] find checker function for a specific target entity *)
      let find_kind_checker ?ctx kind =
        let%lwt rules = Rule.find_all ?ctx (TargetSpec.Entity kind) in
        Lwt.return_ok @@ Utils.exists_in rules
      ;;

      let find_any_kind_checker ?ctx kind =
        let%lwt rules = Rule.find_all_of_entity ?ctx (TargetSpec.Entity kind) in
        Lwt.return_ok @@ Utils.exists_in rules
      ;;
    end

    (** [expand_set] For transitional effects, uses the registered dependencies
        to look for the parent object. Update and return the passed
        [validation_set] *)
    let expand_set ?ctx (validation_set : ValidationSet.t)
      : ValidationSet.t Lwt.t
      =
      let open Lwt.Infix in
      let rec expand set : validation_set Lwt.t =
        let open ValidationSet in
        let find_all = Lwt_list.map_s expand in
        match set with
        | One effect ->
          Relation.find_effects_rec ?ctx effect
          >|= (function
          | [] ->
            Logs.debug ~src (fun m ->
              m "Expand: %s (No parents) " ([%show: Effect.t] effect));
            One effect
          | parent_effects ->
            Logs.debug ~src (fun m ->
              m
                "Expand: %s with parent %s "
                ([%show: Effect.t] effect)
                ([%show: Effect.t list] parent_effects));
            One effect :: CCList.map one parent_effects |> or_)
        | Or effects -> effects |> find_all >|= or_
        | And effects -> effects |> find_all >|= and_
        | SpecificRole role -> SpecificRole role |> Lwt.return
        | NotRole role -> NotRole role |> Lwt.return
      in
      validation_set |> expand
    ;;

    (** [validate ?ctx error validation_set actor] checks permissions and
        gracefully reports authorization errors.

        [?any_id] validation checks against any element of a specific ID or the
        entity itself, 'Read Entity XY' and 'Read Id (XY, uuid)' are both valid

        [error] e.g. to change the error type to the one used in your app (e.g.
        `CCFun.id` to keep the string type)

        [validation_set] effect set to check the permissions against

        [actor] actor object who'd like to perform the action *)
    let validate
      ?ctx
      ?(any_id = false)
      (error : string -> 'etyp)
      (validation_set : ValidationSet.t)
      (actor : 'a actor)
      : (unit, 'etyp) Lwt_result.t
      =
      let open Lwt_result.Infix in
      let ( >>> ) = Lwt_result.bind_result in
      let map_error = Lwt_result.map_error error in
      let rec find_checker =
        let open ValidationSet in
        let find ((action, spec) as effect : Action.t * TargetSpec.t) =
          let log_debug valid =
            Logs.debug ~src (fun m ->
              m
                "Validated: %s (%s)"
                (string_of_bool valid)
                ([%show: Effect.t] effect))
          in
          (match spec with
           | (TargetSpec.Id (kind, _) | TargetSpec.Entity kind) when any_id ->
             Target.find_any_kind_checker ?ctx kind
           | TargetSpec.Id (kind, uuid) ->
             Target.(find ?ctx kind uuid >>= find_checker ?ctx)
           | TargetSpec.Entity kind -> Target.find_kind_checker ?ctx kind)
          >|= (fun checker_fcn -> checker_fcn actor action)
          >|= tap log_debug
        in
        function
        | One effect -> find effect
        | SpecificRole role ->
          Actor.find_roles ?ctx (Actor.id actor) >|= RoleSet.(mem role)
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
        | NotRole role ->
          Actor.find_roles ?ctx (Actor.id actor) >|= RoleSet.(mem role) >|= not
        | Or [] | And [] -> Lwt.return_ok true
      in
      let validate = function
        | true -> Ok ()
        | false ->
          Error
            (Format.asprintf
               "Entity %s: Permission denied for %s"
               ([%show: Actor.t] actor)
               ([%show: ValidationSet.t] validation_set))
      in
      validation_set
      |> expand_set ?ctx
      |> Lwt_result.ok
      >>= find_checker
      >>> validate
      |> map_error
    ;;

    (** [wrap_function ?ctx error validation_set f] produces a wrapped version
        of [f] which checks permissions and gracefully reports authorization
        errors.

        [error] e.g. to change the error type to the one used in your app (e.g.
        `CCFun.id` to keep the string type)

        [validation_set] effect set to check the permissions against *)
    let wrap_function
      ?ctx
      (error : string -> 'etyp)
      (validation_set : ValidationSet.t)
      (fcn : 'param -> ('rval, 'etyp) Lwt_result.t)
      =
      let open Lwt_result.Syntax in
      let can = validate ?ctx error validation_set in
      Lwt.return_ok (fun actor param ->
        let* () = can actor in
        fcn param)
    ;;
  end
end
