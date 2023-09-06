open CCFun.Infix

let src = Logs.Src.create "guardian"

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
      CCList.fold_left
        (fun init ({ permission; model; target_uuid } as permission_on_target) ->
          let is_manage_model () =
            equal
              (of_tuple (Permission.Manage, model, None))
              permission_on_target
          in
          let model_permission () =
            let in_list perm =
              CCList.mem ~eq:equal (of_tuple (perm, model, None)) perms
            in
            in_list permission || in_list Permission.Manage
          in
          let manage_permission () =
            CCList.mem (of_tuple (Permission.Manage, model, target_uuid)) perms
          in
          match target_uuid with
          | None when is_manage_model () -> init @ [ permission_on_target ]
          | None when manage_permission () -> init
          | None -> init @ [ permission_on_target ]
          | Some _
            when Permission.(equal Manage permission)
                 && model_permission () |> not ->
            init @ [ permission_on_target ]
          | Some _ when model_permission () || manage_permission () -> init
          | Some _ -> init @ [ permission_on_target ])
        []
        perms
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
        | Some u1, Some u2 -> Uuid.Target.equal u1 u2
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

  module type PersistenceSig =
    Persistence.Contract
      with type actor = Actor.t
       and type actor_model = ActorModel.t
       and type actor_role = ActorRole.t
       and type actor_permission = ActorPermission.t
       and type permission_on_target = PermissionOnTarget.t
       and type role = Role.t
       and type role_permission = RolePermission.t
       and type target = Target.t
       and type target_entity = TargetEntity.t
       and type target_model = TargetModel.t
       and type validation_set = ValidationSet.t

  module MakePersistence
      (Backend : Persistence.Backend
                   with type actor = Actor.t
                    and type actor_model = ActorModel.t
                    and type actor_role = ActorRole.t
                    and type actor_permission = ActorPermission.t
                    and type permission_on_target = PermissionOnTarget.t
                    and type role = Role.t
                    and type role_permission = RolePermission.t
                    and type target = Target.t
                    and type target_entity = TargetEntity.t
                    and type target_model = TargetModel.t
                    and type validation_set = ValidationSet.t) :
    PersistenceSig = struct
    include Backend

    let clear_cache () = Repo.clear_cache ()

    module RolePermission = struct
      include Repo.RolePermission

      let insert_all ?ctx =
        Lwt_list.fold_left_s
          (fun acc x ->
            match%lwt insert ?ctx x with
            | Ok () -> CCResult.map (CCList.cons x) acc |> Lwt_result.lift
            | Error (_ : string) ->
              CCResult.map_err (CCList.cons x) acc |> Lwt_result.lift)
          (Ok [])
      ;;
    end

    module ActorPermission = struct
      include Repo.ActorPermission

      let insert_all ?ctx =
        Lwt_list.fold_left_s
          (fun acc x ->
            match%lwt insert ?ctx x with
            | Ok () -> CCResult.map (CCList.cons x) acc |> Lwt_result.lift
            | Error (_ : string) ->
              CCResult.map_err (CCList.cons x) acc |> Lwt_result.lift)
          (Ok [])
      ;;
    end

    module Actor = struct
      include Actor
      include Repo.Actor

      (** [decorate ?ctx to_actor] This convenience function should be used to
          decorate the [actor] * functions of authorizable modules. The newly
          decorated function connects * to the persistent backend to ensure that
          the authorizable's roles and ownership * are consistent in both
          spaces. *)
      let decorate ?ctx (to_actor : 'a -> actor)
        : 'a -> (actor, string) Lwt_result.t
        =
        fun x ->
        let open Lwt_result.Syntax in
        let ({ Actor.uuid; _ } as entity : actor) = to_actor x in
        let* mem = mem ?ctx uuid in
        if mem
        then find ?ctx uuid
        else
          let* () = insert ?ctx entity in
          Lwt.return_ok entity
      ;;
    end

    module ActorRole = struct
      include ActorRole
      include Repo.ActorRole
    end

    module Target = struct
      include Target
      include Repo.Target

      (** [decorate ?ctx to_target] This convenience function should be used to
          decorate the [target] * functions of authorizable modules. The newly
          decorated function connects * to the persistent backend to ensure that
          the authorizable's roles and ownership * are consistent in both
          spaces. *)
      let decorate ?ctx (to_target : 'a -> target)
        : 'a -> (target, string) Lwt_result.t
        =
        fun x ->
        let open Lwt_result.Syntax in
        let ({ Target.uuid; _ } as entity : target) = to_target x in
        let* mem = mem ?ctx uuid in
        if mem
        then find ?ctx uuid
        else
          let* () = insert ?ctx entity in
          Lwt.return_ok entity
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
