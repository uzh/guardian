open CCFun.Infix

type context = Persistence.context

module type RoleSig = Role.Sig

module Make (ActorModel : RoleSig) (Role : RoleSig) (TargetModel : RoleSig) =
struct
  module Uuid = Uuid
  module Permission = Permission

  module TargetEntity = struct
    type t =
      | Model of TargetModel.t
      | Id of Uuid.Target.t
    [@@deriving eq, show, ord, yojson, sexp_of]

    let model m = Model m
    let id uuid = Id uuid

    let is_id = function
      | Id _ -> true
      | Model _ -> false
    ;;

    let find_id = function
      | Id uuid -> Some uuid
      | Model _ -> None
    ;;
  end

  module Actor = struct
    type t =
      { uuid : Uuid.Actor.t
      ; model : ActorModel.t
      }
    [@@deriving eq, show, ord, yojson, sexp_of]

    let create model uuid = { uuid; model }
  end

  module type ActorSig = sig
    type t

    (** [to_authorizable x] converts [x] to a uniquely identifiable object,
        complete * with roles. The [authorizable] can't be converted back into
        type [t]. **)
    val to_authorizable : ?ctx:context -> t -> (Actor.t, string) Lwt_result.t
  end

  module ActorRole = struct
    type t =
      { actor_uuid : Uuid.Actor.t
      ; role : Role.t
      ; target_uuid : Uuid.Target.t option [@sexp.option]
      }
    [@@deriving eq, show, ord, yojson, sexp_of]

    let create ?target_uuid actor_uuid role = { actor_uuid; role; target_uuid }

    let role_to_human { role; target_uuid; _ } =
      Role.show role
      :: CCOption.map_or
           ~default:[]
           (Uuid.Target.to_string %> Format.asprintf "(%s)" %> CCList.return)
           target_uuid
      |> CCString.concat " "
    ;;
  end

  module Target = struct
    type t =
      { uuid : Uuid.Target.t
      ; model : TargetModel.t
      }
    [@@deriving eq, show, ord, yojson, sexp_of]

    let create model uuid = { uuid; model }
  end

  module type TargetSig = sig
    type t

    (** [to_authorizable x] converts [x] to a uniquely identifiable object,
        complete * with roles. The [authorizable] may not, however, be converted
        back into type [t]. **)
    val to_authorizable : ?ctx:context -> t -> (Target.t, string) Lwt_result.t
  end

  module RolePermission = struct
    type t =
      { role : Role.t
      ; permission : Permission.t
      ; model : TargetModel.t
      }
    [@@deriving eq, show, ord, yojson, sexp_of]

    let create role permission model = { role; permission; model }
  end

  module ActorPermission = struct
    type t =
      { actor_uuid : Uuid.Actor.t
      ; permission : Permission.t
      ; target : TargetEntity.t
      }
    [@@deriving eq, show, ord, yojson, sexp_of]

    let create_for_model uuid permission model =
      { actor_uuid = uuid; permission; target = TargetEntity.Model model }
    ;;

    let create_for_id uuid permission target_uuid =
      { actor_uuid = uuid; permission; target = TargetEntity.Id target_uuid }
    ;;
  end

  module PermissionOnTarget = struct
    type t =
      { permission : Permission.t
      ; model : TargetModel.t
      ; target_uuid : Uuid.Target.t option [@sexp.option]
      }
    [@@deriving eq, show, ord, yojson, sexp_of]

    let create ?target_uuid permission model =
      { permission; model; target_uuid }
    ;;

    let of_tuple (permission, model, target_uuid) =
      { permission; model; target_uuid }
    ;;

    let filter_permission_on_model filter_permission filter_model =
      CCList.filter (fun { permission; model; _ } ->
        Permission.(
          equal filter_permission permission || equal Manage permission)
        && TargetModel.equal filter_model model)
    ;;

    let remove_duplicates (perms : t list) : t list =
      let module PotSet = CCSet.Make (struct
          type nonrec t = t

          let compare = compare
        end)
      in
      (* Membership indexes over the whole input — grants without a target
         uuid and per-uuid Manage grants — so each element is judged in
         O(log n) instead of rescanning the list. *)
      let model_wide, manage_on_uuid =
        CCList.fold_left
          (fun (model_wide, manage_on_uuid)
            ({ permission; target_uuid; _ } as perm) ->
             match target_uuid with
             | None -> PotSet.add perm model_wide, manage_on_uuid
             | Some _ when Permission.(equal Manage permission) ->
               model_wide, PotSet.add perm manage_on_uuid
             | Some _ -> model_wide, manage_on_uuid)
          (PotSet.empty, PotSet.empty)
          perms
      in
      CCList.fold_left
        (fun init
          ({ permission; model; target_uuid } as permission_on_target) ->
           let is_manage_model () =
             equal
               (of_tuple (Permission.Manage, model, None))
               permission_on_target
           in
           let model_permission () =
             let in_index perm =
               PotSet.mem (of_tuple (perm, model, None)) model_wide
             in
             in_index permission || in_index Permission.Manage
           in
           let manage_permission () =
             let manage = of_tuple (Permission.Manage, model, target_uuid) in
             match target_uuid with
             | None -> PotSet.mem manage model_wide
             | Some _ -> PotSet.mem manage manage_on_uuid
           in
           match target_uuid with
           | None when is_manage_model () -> permission_on_target :: init
           | None when manage_permission () -> init
           | None -> permission_on_target :: init
           | Some _
             when Permission.(equal Manage permission)
                  && model_permission () |> not -> permission_on_target :: init
           | Some _ when model_permission () || manage_permission () -> init
           | Some _ -> permission_on_target :: init)
        []
        perms
      |> CCList.rev
    ;;

    let validate ?(any_id = false) =
      let eq pot1 pot2 =
        Permission.(
          equal pot1.permission pot2.permission || equal Manage pot2.permission)
        &&
        match pot1.target_uuid, pot2.target_uuid with
        | None, Some _ when any_id -> TargetModel.equal pot1.model pot2.model
        | None, Some _ -> false
        | Some _, _ when any_id -> TargetModel.equal pot1.model pot2.model
        | Some u1, Some u2 ->
          TargetModel.equal pot1.model pot2.model && Uuid.Target.equal u1 u2
        | None, None | Some _, None -> TargetModel.equal pot1.model pot2.model
      in
      CCList.mem ~eq
    ;;

    let permission_of_model permission model =
      filter_permission_on_model permission model
      %> CCList.fold_left
           (fun (init, uuids) { target_uuid; _ } ->
              match target_uuid with
              | Some uuid -> init, uuid :: uuids
              | None -> true, uuids)
           (false, [])
    ;;
  end

  module ValidationSet = struct
    type t =
      | And of t list [@sexp.list]
      | Or of t list [@sexp.list]
      | One of PermissionOnTarget.t
    [@@deriving eq, show, ord, yojson, sexp_of]

    let and_ m = And m
    let or_ m = Or m
    let one m = One m
    let one_of_tuple = PermissionOnTarget.of_tuple %> one
    let empty = Or []
  end

  module RoleAssignment = struct
    type t =
      { role : Role.t
      ; target_role : Role.t
      }
    [@@deriving eq, show, ord, yojson, sexp_of]

    let create role target_role = { role; target_role }
  end

  module type PersistenceSig =
    Persistence.Contract
    with type actor = Actor.t
     and type actor_model = ActorModel.t
     and type actor_role = ActorRole.t
     and type actor_permission = ActorPermission.t
     and type permission_on_target = PermissionOnTarget.t
     and type role = Role.t
     and type role_assignment = RoleAssignment.t
     and type role_permission = RolePermission.t
     and type target = Target.t
     and type target_entity = TargetEntity.t
     and type target_model = TargetModel.t
     and type validation_set = ValidationSet.t

  module MakePersistence
      (Backend :
         Persistence.Backend
         with type actor = Actor.t
          and type actor_model = ActorModel.t
          and type actor_role = ActorRole.t
          and type actor_permission = ActorPermission.t
          and type permission_on_target = PermissionOnTarget.t
          and type role = Role.t
          and type role_assignment = RoleAssignment.t
          and type role_permission = RolePermission.t
          and type target = Target.t
          and type target_entity = TargetEntity.t
          and type target_model = TargetModel.t
          and type validation_set = ValidationSet.t) : PersistenceSig = struct
    include Backend

    let clear_cache () = Repo.clear_cache ()

    let insert_all_items insert ?ctx items =
      let%lwt successes, failures =
        Lwt_list.fold_left_s
          (fun (ok, err) x ->
             match%lwt insert ?ctx x with
             | Ok () -> Lwt.return (x :: ok, err)
             | Error (_ : string) -> Lwt.return (ok, x :: err))
          ([], [])
          items
      in
      match failures with
      | [] -> Lwt_result.return (CCList.rev successes)
      | _ -> Lwt_result.fail (CCList.rev failures)
    ;;

    (** [decorate_entity get_uuid insert find to_entity ?ctx x] is the shared
        implementation for [Actor.decorate] and [Target.decorate]. It upserts
        the entity (idempotent) and returns the current DB state, removing the
        need for a separate existence check. *)
    let decorate_entity get_uuid insert find ?ctx to_entity x =
      let open Lwt_result.Syntax in
      let entity = to_entity x in
      let* () = insert ?ctx entity in
      find ?ctx (get_uuid entity)
    ;;

    module RolePermission = struct
      include Repo.RolePermission

      let insert_all ?ctx = insert_all_items insert ?ctx
    end

    module ActorPermission = struct
      include Repo.ActorPermission

      let insert_all ?ctx = insert_all_items insert ?ctx
    end

    module Actor = struct
      include Actor
      include Repo.Actor

      (** [decorate ?ctx to_actor] ensures the actor exists in the persistent
          backend (idempotent upsert) and returns the current stored state. *)
      let decorate ?ctx =
        decorate_entity (fun (e : actor) -> e.Actor.uuid) insert find ?ctx
      ;;
    end

    module ActorRole = struct
      include ActorRole
      include Repo.ActorRole
    end

    module Target = struct
      include Target
      include Repo.Target

      (** [decorate ?ctx to_target] ensures the target exists in the persistent
          backend (idempotent upsert) and returns the current stored state. *)
      let decorate ?ctx =
        decorate_entity (fun (e : target) -> e.Target.uuid) insert find ?ctx
      ;;
    end

    module PermissionOnTarget = struct
      include PermissionOnTarget

      let validate_set
            ?any_id
            perms
            (error : string -> 'etyp)
            (validation_set : ValidationSet.t)
            actor
        =
        let open CCFun in
        let rec find_checker : validation_set -> bool =
          let open ValidationSet in
          function
          | One { PermissionOnTarget.permission; model; target_uuid } ->
            (match target_uuid with
             | Some target_uuid ->
               validate
                 ?any_id
                 (PermissionOnTarget.create ~target_uuid permission model)
                 perms
             | None ->
               validate
                 ?any_id
                 (PermissionOnTarget.create permission model)
                 perms)
          | Or (rule :: rules) ->
            (match find_checker rule with
             | true -> true
             | false ->
               CCList.fold_left
                 (flip (fun rule -> function
                    | true -> true
                    | false -> find_checker rule))
                 false
                 rules)
          | And (rule :: rules) ->
            (match find_checker rule with
             | false -> false
             | true ->
               CCList.fold_left
                 (flip (fun rule -> function
                    | true -> find_checker rule
                    | false -> false))
                 true
                 rules)
          | Or [] | And [] -> true
        in
        let validate = function
          | true -> Ok ()
          | false ->
            Error
              (Utils.deny_message_validation_set
                 actor.Actor.uuid
                 ([%show: ValidationSet.t] validation_set))
        in
        validation_set |> find_checker |> validate |> CCResult.map_err error
      ;;
    end

    module RoleAssignment = struct
      include RoleAssignment
      include Repo.RoleAssignment

      (** [can_assign_roles ?ctx role] returns all roles which can be assigned by
          the provided role *)
      let can_assign_roles ?ctx =
        Repo.RoleAssignment.find_all_by_role ?ctx
        %> Lwt.map (CCList.map (fun ra -> ra.target_role))
      ;;
    end

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
          ?any_id
          (error : string -> 'etyp)
          (validation_set : ValidationSet.t)
          actor
      : (unit, 'etyp) Lwt_result.t
      =
      let open CCFun in
      let ( |>> ) = flip Lwt.map in
      let rec find_checker =
        let open ValidationSet in
        function
        | One { PermissionOnTarget.permission; model; target_uuid } ->
          Repo.validate ?ctx ?any_id ?target_uuid ~model permission actor
        | Or (rule :: rules) ->
          (match%lwt find_checker rule with
           | true -> Lwt.return_true
           | false ->
             Lwt_list.fold_left_s
               (flip (fun rule -> function
                  | true -> Lwt.return_true
                  | false -> find_checker rule))
               false
               rules)
        | And (rule :: rules) ->
          (match%lwt find_checker rule with
           | false -> Lwt.return_false
           | true ->
             Lwt_list.fold_left_s
               (flip (fun rule -> function
                  | true -> find_checker rule
                  | false -> Lwt.return_false))
               true
               rules)
        | Or [] | And [] -> Lwt.return_true
      in
      let validate = function
        | true -> Ok ()
        | false ->
          Error
            (Utils.deny_message_validation_set
               actor.Actor.uuid
               ([%show: ValidationSet.t] validation_set))
      in
      validation_set |> find_checker |>> validate |> Lwt_result.map_error error
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
