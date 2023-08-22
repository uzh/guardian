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
    [@@deriving eq, show, ord, yojson]

    let model m = Model m
    let id uuid = Id uuid
  end

  module Actor = struct
    type t =
      { uuid : Uuid.Actor.t
      ; model : ActorModel.t
      }
    [@@deriving eq, show, ord, yojson]

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
      ; target_uuid : Uuid.Target.t option
      }
    [@@deriving eq, show, ord, yojson]

    let create ?target_uuid actor_uuid role = { actor_uuid; role; target_uuid }
  end

  module Target = struct
    type t =
      { uuid : Uuid.Target.t
      ; model : TargetModel.t
      }
    [@@deriving eq, show, ord, yojson]

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
    [@@deriving eq, show, ord, yojson]

    let create role permission model = { role; permission; model }
  end

  module ActorPermission = struct
    type t =
      { actor_uuid : Uuid.Actor.t
      ; permission : Permission.t
      ; target : TargetEntity.t
      }
    [@@deriving eq, show, ord, yojson]

    let create_for_model uuid permission model =
      { actor_uuid = uuid; permission; target = TargetEntity.Model model }
    ;;

    let create_for_id uuid permission target_uuid =
      { actor_uuid = uuid; permission; target = TargetEntity.Id target_uuid }
    ;;
  end

  module ValidationSet = struct
    type t =
      | And of t list
      | Or of t list
      | One of Permission.t * TargetEntity.t
    [@@deriving eq, show, ord, yojson]

    let and_ m = And m
    let or_ m = Or m
    let one (m, k) = One (m, k)
    let empty = Or []
  end

  module type PersistenceSig =
    Persistence.Contract
      with type actor = Actor.t
       and type actor_model = ActorModel.t
       and type actor_role = ActorRole.t
       and type actor_permission = ActorPermission.t
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
        | One (permission, TargetEntity.Id uuid) ->
          Repo.validate ?ctx ?any_id ~target_uuid:uuid permission actor
        | One (permission, TargetEntity.Model model) ->
          Repo.validate ?ctx ?any_id ~model permission actor
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

    let exists =
      let eq (p1, e1) (p2, e2) =
        Permission.(equal p1 p2 || equal Manage p2) && TargetEntity.equal e1 e2
      in
      CCList.mem ~eq
    ;;
  end
end
