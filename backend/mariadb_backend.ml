open CCFun
open Lwt.Infix
open Caqti_request.Infix

module Make
    (ActorModel : Guardian.RoleSig)
    (Role : Guardian.RoleSig)
    (TargetModel : Guardian.RoleSig)
    (Database : Database_pools.Sig) =
struct
  module Guard = Guardian.Make (ActorModel) (Role) (TargetModel)

  let lowercase_role =
    CCString.(TargetModel.show %> replace ~sub:"`" ~by:"" %> lowercase_ascii)
  ;;

  let capitalize_role =
    CCString.(TargetModel.show %> replace ~sub:"`" ~by:"" %> capitalize_ascii)
  ;;

  module Entity = struct
    module Uuid = struct
      module UuidBase (Core : Guard.Uuid.Sig) = struct
        include Core

        let t =
          Caqti_type.(
            custom
              ~encode:(to_string %> CCResult.return)
              ~decode:(fun id ->
                id
                |> of_string
                |> CCOption.to_result (Format.asprintf "Invalid UUID: %s" id))
              string)
        ;;
      end

      module Actor = UuidBase (Guard.Uuid.Actor)
      module Target = UuidBase (Guard.Uuid.Target)
    end

    module Role = struct
      include Role

      let t =
        let open CCResult in
        Caqti_type.(
          custom
            ~encode:(Role.show %> return)
            ~decode:(of_string %> return)
            string)
      ;;
    end

    module ActorModel = struct
      include ActorModel

      let t =
        let open CCResult in
        Caqti_type.(
          custom
            ~encode:(ActorModel.show %> return)
            ~decode:(of_string %> return)
            string)
      ;;
    end

    module TargetModel = struct
      include TargetModel

      let t =
        let open CCResult in
        Caqti_type.(
          custom
            ~encode:(TargetModel.show %> return)
            ~decode:(of_string %> return)
            string)
      ;;
    end

    module Permission = struct
      include Guard.Permission

      let t =
        let open CCResult in
        Caqti_type.(
          custom
            ~encode:(Guard.Permission.show %> return)
            ~decode:(of_string %> return)
            string)
      ;;
    end

    module ActorRole = struct
      include Guard.ActorRole

      let targets =
        let encode m =
          let open CCResult in
          m.target_uuid
          |> CCOption.to_result "Missing target_uuid"
          >|= fun target_uuid -> m.actor_uuid, m.role, target_uuid
        in
        let decode (actor_uuid, role, target_uuid) =
          Ok { actor_uuid; role; target_uuid = Some target_uuid }
        in
        Caqti_type.(
          custom ~encode ~decode (tup3 Uuid.Actor.t Role.t Uuid.Target.t))
      ;;

      let role =
        let encode m =
          let open CCResult in
          (match m.target_uuid with
           | Some _ -> Error "target_uuid defined for role only model"
           | None -> Ok ())
          >|= fun () -> m.actor_uuid, m.role
        in
        let decode (actor_uuid, role) =
          Ok { actor_uuid; role; target_uuid = None }
        in
        Caqti_type.(custom ~encode ~decode (tup2 Uuid.Actor.t Role.t))
      ;;

      let t =
        let encode _ = Error "Read only model of ActorRoles" in
        let decode (actor_uuid, role, target_uuid) =
          Ok { actor_uuid; role; target_uuid }
        in
        Caqti_type.(
          custom
            ~encode
            ~decode
            (tup3 Uuid.Actor.t Role.t (option Uuid.Target.t)))
      ;;
    end

    module Actor = struct
      include Guard.Actor

      let t =
        let encode m = Ok (m.uuid, m.model) in
        let decode (uuid, model) = Ok { uuid; model } in
        Caqti_type.(custom ~encode ~decode (tup2 Uuid.Actor.t ActorModel.t))
      ;;
    end

    module Target = struct
      include Guard.Target

      let t =
        let encode m = Ok (m.uuid, m.model) in
        let decode (uuid, model) = Ok { uuid; model } in
        Caqti_type.(custom ~encode ~decode (tup2 Uuid.Target.t TargetModel.t))
      ;;
    end

    module TargetEntity = struct
      include Guard.TargetEntity

      let t =
        let open Guard.TargetEntity in
        let encode = function
          | Id uuid -> Ok (None, Some uuid)
          | Model model -> Ok (Some model, None)
        in
        let decode (model, uuid) =
          match model, uuid with
          | None, None ->
            Error
              "Invalid actor permission, either model or target uuid need to \
               be set"
          | Some _, Some _ ->
            Error
              "Invalid actor permission, only one of model and target uuid \
               need to be set"
          | Some model, None -> Ok (Model model)
          | None, Some uuid -> Ok (Id uuid)
        in
        Caqti_type.(
          custom
            ~encode
            ~decode
            (tup2 (option TargetModel.t) (option Uuid.Target.t)))
      ;;
    end

    module RolePermission = struct
      include Guard.RolePermission

      let t =
        let encode m = Ok (m.role, m.permission, m.model) in
        let decode (role, permission, model) = Ok { role; permission; model } in
        Caqti_type.(
          custom ~encode ~decode (tup3 Role.t Permission.t TargetModel.t))
      ;;
    end

    module ActorPermission = struct
      include Guard.ActorPermission

      let t =
        let encode m = Ok (m.actor_uuid, m.permission, m.target) in
        let decode (actor_uuid, permission, target) =
          Ok { actor_uuid; permission; target }
        in
        Caqti_type.(
          custom ~encode ~decode (tup3 Uuid.Actor.t Permission.t TargetEntity.t))
      ;;
    end

    module PermissionOnTarget = struct
      include Guard.PermissionOnTarget

      let t =
        let encode m = Ok (m.permission, m.model, m.target_uuid) in
        let decode (permission, model, target_uuid) =
          Ok { permission; model; target_uuid }
        in
        Caqti_type.(
          custom
            ~encode
            ~decode
            (tup3 Permission.t TargetModel.t (option Uuid.Target.t)))
      ;;
    end
  end

  module DBCache = struct
    open CCCache

    let equal_validate (c1, any1, a1, p1, pt1) (c2, any2, a2, p2, pt2) =
      let ctx = [%show: (string * string) list] in
      CCOption.equal (fun a b -> CCString.equal (ctx a) (ctx b)) c1 c2
      && CCOption.equal CCBool.equal any1 any2
      && Guard.Uuid.Actor.equal a1 a2
      && Guard.Permission.equal p1 p2
      && Guard.TargetEntity.equal pt1 pt2
    ;;

    let lru_validate
      : ( (string * string) list option
          * bool option
          * Guard.Uuid.Actor.t
          * Guard.Permission.t
          * Guard.TargetEntity.t
          , bool )
          t
      =
      lru ~eq:equal_validate 16384
    ;;

    let clear () = clear lru_validate
  end

  include Guard.MakePersistence (struct
      type actor = Guard.Actor.t
      type actor_model = ActorModel.t
      type actor_permission = Guard.ActorPermission.t
      type actor_role = Guard.ActorRole.t
      type permission_on_target = Guard.PermissionOnTarget.t
      type role = Role.t
      type role_permission = Guard.RolePermission.t
      type target = Guard.Target.t
      type target_entity = Guard.TargetEntity.t
      type target_model = TargetModel.t
      type validation_set = Guard.ValidationSet.t

      module Repo = struct
        let clear_cache = DBCache.clear

        let define_encode_uuid =
          let function_name = "guardianEncodeUuid" in
          ( function_name
          , Format.asprintf
              {sql|
              CREATE FUNCTION %s(uuid VARCHAR(36))
                RETURNS BINARY(16)
                BEGIN
                  RETURN UNHEX(REPLACE(uuid, '-', ''));
                END
            |sql}
              function_name )
        ;;

        let define_decode_uuid =
          let function_name = "guardianDecodeUuid" in
          ( function_name
          , Format.asprintf
              {sql|
              CREATE FUNCTION %s(uuid BINARY(16))
                RETURNS VARCHAR(36)
                BEGIN
                  RETURN LOWER(CONCAT(
                    SUBSTR(HEX(uuid), 1, 8), '-',
                    SUBSTR(HEX(uuid), 9, 4), '-',
                    SUBSTR(HEX(uuid), 13, 4), '-',
                    SUBSTR(HEX(uuid), 17, 4), '-',
                    SUBSTR(HEX(uuid), 21)
                  ));
                END
            |sql}
              function_name )
        ;;

        module ActorRole = struct
          let upsert_uuid_request =
            {sql|
              INSERT INTO guardian_actor_role_targets (actor_uuid, role, target_uuid)
              VALUES (guardianEncodeUuid(?), ?, guardianEncodeUuid(?))
              ON DUPLICATE KEY UPDATE
                mark_as_deleted = NULL,
                updated_at = NOW()
            |sql}
            |> Entity.ActorRole.targets ->. Caqti_type.unit
          ;;

          let upsert_model_request =
            {sql|
              INSERT INTO guardian_actor_roles (actor_uuid, role)
              VALUES (guardianEncodeUuid(?), ?)
              ON DUPLICATE KEY UPDATE
                mark_as_deleted = NULL,
                updated_at = NOW()
            |sql}
            |> Entity.ActorRole.role ->. Caqti_type.unit
          ;;

          let upsert ?ctx ({ Entity.ActorRole.target_uuid; _ } as role) =
            let () = clear_cache () in
            match target_uuid with
            | Some _ -> Database.exec ?ctx upsert_uuid_request role
            | None -> Database.exec ?ctx upsert_model_request role
          ;;

          let find_by_actor_request =
            {sql|
              SELECT
                guardianDecodeUuid(role_targets.actor_uuid),
                role_targets.role,
                guardianDecodeUuid(role_targets.target_uuid)
              FROM guardian_actor_role_targets AS role_targets
              WHERE role_targets.actor_uuid = guardianEncodeUuid($1)
                AND role_targets.mark_as_deleted IS NULL
              UNION
              SELECT guardianDecodeUuid(roles.actor_uuid), roles.role, NULL
              FROM guardian_actor_roles AS roles
              WHERE roles.actor_uuid = guardianEncodeUuid($1)
                AND roles.mark_as_deleted IS NULL
            |sql}
            |> Entity.(Uuid.Actor.t ->* ActorRole.t)
          ;;

          let find_by_actor ?ctx = Database.collect ?ctx find_by_actor_request

          let find_by_target_request =
            {sql|
              SELECT
                guardianDecodeUuid(role_targets.actor_uuid),
                role_targets.role,
                guardianDecodeUuid(role_targets.target_uuid)
              FROM guardian_actor_role_targets AS role_targets
              WHERE role_targets.role = $1
                AND role_targets.target_uuid = guardianEncodeUuid($2)
                AND role_targets.mark_as_deleted IS NULL
              UNION
              SELECT guardianDecodeUuid(roles.actor_uuid), roles.role, NULL
              FROM guardian_actor_roles AS roles
              WHERE roles.role = $1
                AND roles.mark_as_deleted IS NULL
            |sql}
            |> Entity.(Caqti_type.tup2 Role.t Uuid.Target.t ->* ActorRole.t)
          ;;

          let find_by_target ?ctx = Database.collect ?ctx find_by_target_request

          let create_exclude
            ?(field = "roles.actor_uuid")
            ?(dynparam = Guardian.Utils.Dynparam.empty)
            ?(with_uuid = false)
            exclude
            =
            let open Guardian.Utils.Dynparam in
            if CCList.is_empty exclude
            then dynparam, ""
            else (
              let arguments, params =
                CCList.fold_left
                  (fun (args, dyn) (role, target_uuid) ->
                    match target_uuid with
                    | None when with_uuid ->
                      ( "(exclude.role = ? AND exclude.target_uuid IS NULL)"
                        :: args
                      , dyn |> add Entity.Role.t role )
                    | None ->
                      ( "exclude.role = ? AND exclude.target_uuid IS NULL"
                        :: args
                      , dyn |> add Entity.Role.t role )
                    | Some uuid ->
                      ( "(exclude.role = ? AND exclude.target_uuid = \
                         guardianEncodeUuid(?))"
                        :: args
                      , dyn
                        |> add Entity.Role.t role
                        |> add Entity.Uuid.Target.t uuid ))
                  ([], dynparam)
                  exclude
              in
              ( params
              , Format.asprintf
                  {sql|AND %s NOT IN (
                    SELECT actor_uuid
                    FROM (
                      SELECT actor_uuid, role, target_uuid FROM guardian_actor_role_targets
                      WHERE mark_as_deleted IS NULL
                      UNION
                      SELECT actor_uuid, role, NULL FROM guardian_actor_roles
                      WHERE mark_as_deleted IS NULL
                      ) AS exclude
                    WHERE %s)
                  |sql}
                  field
                  (CCString.concat "\nAND " arguments) ))
          ;;

          let find_actors_by_role_request ?(exclude_sql = "") params =
            Format.asprintf
              {sql|
                SELECT guardianDecodeUuid(roles.actor_uuid)
                FROM guardian_actor_roles AS roles
                WHERE roles.role = ?
                  AND roles.mark_as_deleted IS NULL
                  %s
              |sql}
              exclude_sql
            |> params ->* Entity.Uuid.Actor.t
          ;;

          let find_actors_by_target_request ?(exclude_sql = "") params =
            Format.asprintf
              {sql|
                SELECT guardianDecodeUuid(role_targets.actor_uuid)
                FROM guardian_actor_role_targets AS role_targets
                WHERE role_targets.target_uuid = guardianEncodeUuid(?)
                  AND role_targets.mark_as_deleted IS NULL
                  AND role_targets.role = ?
                  %s
              |sql}
              exclude_sql
            |> params ->* Entity.Uuid.Actor.t
          ;;

          let find_actors_by_role ?ctx ?(exclude = []) (role, target_uuid) =
            let open Guardian.Utils.Dynparam in
            match target_uuid with
            | Some uuid ->
              let field = "role_targets.actor_uuid" in
              let dynparam =
                empty |> add Entity.Uuid.Target.t uuid |> add Entity.Role.t role
              in
              let Pack (pt, pv), exclude_sql =
                create_exclude ~field ~dynparam ~with_uuid:true exclude
              in
              Database.collect
                ?ctx
                (find_actors_by_target_request ~exclude_sql pt)
                pv
            | None ->
              let field = "roles.actor_uuid" in
              let dynparam = empty |> add Entity.Role.t role in
              let Pack (pt, pv), exclude_sql =
                create_exclude ~field ~dynparam exclude
              in
              Database.collect
                ?ctx
                (find_actors_by_role_request ~exclude_sql pt)
                pv
          ;;

          let permissions_of_actor_request =
            let open Entity in
            {sql|
              SELECT
                role_permissions.permission,
                role_permissions.target_model,
                guardianDecodeUuid(roles.target_uuid)
              FROM
                guardian_actor_role_targets AS roles
                JOIN guardian_role_permissions AS role_permissions ON role_permissions.role = roles.role
                  AND role_permissions.mark_as_deleted IS NULL
              WHERE
                roles.mark_as_deleted IS NULL
                AND roles.actor_uuid = guardianEncodeUuid($1)
              UNION
              SELECT
                role_permissions.permission,
                role_permissions.target_model,
                NULL
              FROM
                guardian_actor_roles AS roles
                JOIN guardian_role_permissions AS role_permissions ON role_permissions.role = roles.role
                  AND role_permissions.mark_as_deleted IS NULL
              WHERE
                roles.mark_as_deleted IS NULL
                AND roles.actor_uuid = guardianEncodeUuid($1)
              UNION
              SELECT
                actor_permissions.permission,
                COALESCE (actor_permissions.target_model, targets.model),
                guardianDecodeUuid(actor_permissions.target_uuid)
              FROM
                guardian_actor_permissions AS actor_permissions
              JOIN guardian_targets AS targets
                ON targets.uuid = actor_permissions.target_uuid
                AND targets.mark_as_deleted IS NULL
              WHERE
                actor_permissions.actor_uuid = guardianEncodeUuid($1)
                AND actor_permissions.mark_as_deleted IS NULL
            |sql}
            |> Uuid.Actor.t ->* PermissionOnTarget.t
          ;;

          let permissions_of_actor ?ctx
            : Guard.Uuid.Actor.t -> permission_on_target list Lwt.t
            =
            let open Guard in
            Database.collect ?ctx permissions_of_actor_request
            %> Lwt.map PermissionOnTarget.remove_duplicates
          ;;

          let delete_role_uuid_request =
            Format.asprintf
              {sql|
                UPDATE guardian_actor_role_targets
                SET mark_as_deleted = NOW()
                WHERE actor_uuid = guardianEncodeUuid($1)
                  AND role = $2
                  AND target_uuid = guardianEncodeUuid($3)
              |sql}
            |> Entity.ActorRole.targets ->. Caqti_type.unit
          ;;

          let delete_role_model_request =
            let open Entity in
            {sql|
              UPDATE guardian_actor_roles
              SET mark_as_deleted = NOW()
              WHERE actor_uuid = guardianEncodeUuid($1)
                AND role = $2
            |sql}
            |> Caqti_type.(tup2 Uuid.Actor.t Entity.Role.t ->. unit)
          ;;

          let delete ?ctx role =
            let open Guard.ActorRole in
            let () = clear_cache () in
            match role.target_uuid with
            | Some _ -> Database.exec ?ctx delete_role_uuid_request role
            | None ->
              Database.exec
                ?ctx
                delete_role_model_request
                (role.actor_uuid, role.role)
          ;;
        end

        module RolePermission = struct
          let from_sql =
            {sql| guardian_role_permissions AS role_permissions |sql}
          ;;

          let std_filter_sql =
            {sql| role_permissions.mark_as_deleted IS NULL |sql}
          ;;

          let select_sql =
            {sql|
              role_permissions.role,
              role_permissions.permission,
              role_permissions.target_model
            |sql}
          ;;

          let combine_sql ?(joins = "") ?(where_additions = "") select =
            Format.asprintf
              "SELECT\n  %s\nFROM  %s\n  %s\nWHERE\n  %s\n  AND %s"
              select
              from_sql
              joins
              std_filter_sql
              where_additions
          ;;

          let find_all_request =
            combine_sql select_sql
            |> Caqti_type.unit ->* Entity.RolePermission.t
          ;;

          let find_all ?ctx = Database.collect ?ctx find_all_request

          let find_all_of_model_request =
            let where_additions =
              {sql|role_permissions.target_model = $1|sql}
            in
            combine_sql ~where_additions select_sql
            |> Entity.(TargetModel.t ->* RolePermission.t)
          ;;

          let find_all_of_model ?ctx =
            Database.collect ?ctx find_all_of_model_request
          ;;

          let insert_request =
            let open Entity in
            {sql|
              INSERT INTO guardian_role_permissions (role, permission, target_model)
              VALUES (?, ?, ?) ON
              DUPLICATE KEY UPDATE
                mark_as_deleted = NULL,
                updated_at = NOW()
            |sql}
            |> RolePermission.t ->. Caqti_type.unit
          ;;

          let insert ?ctx =
            let () = clear_cache () in
            Database.exec ?ctx insert_request %> Lwt_result.ok
          ;;

          let delete_request =
            let open Entity in
            {sql|
              UPDATE guardian_role_permissions
              SET mark_as_deleted = NOW()
              WHERE role = ?
                AND permission = ?
                AND target_model = ?
            |sql}
            |> RolePermission.t ->. Caqti_type.unit
          ;;

          let delete ?ctx =
            let () = clear_cache () in
            Database.exec ?ctx delete_request %> Lwt_result.ok
          ;;
        end

        module ActorPermission = struct
          let from_sql =
            {sql| guardian_actor_permissions AS actor_permissions |sql}
          ;;

          let std_filter_sql =
            {sql| actor_permissions.mark_as_deleted IS NULL |sql}
          ;;

          let select_sql =
            {sql|
              actor_permissions.actor_uuid,
              actor_permissions.permission,
              actor_permissions.target_model,
              actor_permissions.target_uuid
            |sql}
          ;;

          let combine_sql ?(joins = "") ?(where_additions = "") select =
            Format.asprintf
              "SELECT\n  %s\nFROM  %s\n  %s\nWHERE\n  %s\n  AND %s"
              select
              from_sql
              joins
              std_filter_sql
              where_additions
          ;;

          let find_all_request =
            combine_sql select_sql
            |> Caqti_type.unit ->* Entity.ActorPermission.t
          ;;

          let find_all ?ctx = Database.collect ?ctx find_all_request

          let find_all_of_uuid_request =
            let joins =
              {sql|JOIN guardian_targets AS targets ON targets.uuid = actor_permissions.target_uuid|sql}
            in
            let where_additions =
              {sql|actor_permissions.target_uuid = $1
                OR actor_permissions.target_model = (SELECT targets.model FROM guardian_targets AS targets WHERE targets.uuid = guardianEncodeUuid($1))
              |sql}
            in
            combine_sql ~joins ~where_additions select_sql
            |> Entity.(Uuid.Target.t ->* ActorPermission.t)
          ;;

          let find_all_of_model_request =
            let joins =
              {sql|JOIN guardian_targets AS targets ON targets.uuid = actor_permissions.target_uuid|sql}
            in
            let where_additions =
              {sql|actor_permissions.target_model = $1
              OR (actor_permissions.target_model IS NULL AND targets.model = $1)
            |sql}
            in
            combine_sql ~joins ~where_additions select_sql
            |> Entity.(TargetModel.t ->* ActorPermission.t)
          ;;

          let find_all_of_entity ?ctx =
            let open Guard.TargetEntity in
            function
            | Model model ->
              Database.collect ?ctx find_all_of_model_request model
            | Id uuid -> Database.collect ?ctx find_all_of_uuid_request uuid
          ;;

          let insert_request =
            {sql|
              INSERT INTO guardian_actor_permissions (actor_uuid, permission, target_model, target_uuid)
              VALUES (guardianEncodeUuid(?), ?, ?, guardianEncodeUuid(?))
              ON DUPLICATE KEY UPDATE
                mark_as_deleted = NULL,
                updated_at = NOW()
            |sql}
            |> Entity.ActorPermission.t ->. Caqti_type.unit
          ;;

          let insert ?ctx =
            let () = clear_cache () in
            Database.exec ?ctx insert_request %> Lwt_result.ok
          ;;

          let delete_request =
            {sql|
              UPDATE guardian_actor_permissions
              SET mark_as_deleted = NOW()
              WHERE actor_uuid = guardianEncodeUuid(?)
                AND permission = ?
                AND target_model = ?
                AND target_uuid = guardianEncodeUuid(?)
            |sql}
            |> Entity.ActorPermission.t ->. Caqti_type.unit
          ;;

          let delete ?ctx =
            let () = clear_cache () in
            Database.exec ?ctx delete_request %> Lwt_result.ok
          ;;
        end

        module Actor = struct
          let not_found =
            [%show: Guard.Uuid.Actor.t]
            %> Format.asprintf "Actor ('%s') not found"
          ;;

          let from_sql = {sql| guardian_actors AS actors |sql}
          let std_filter_sql = {sql| actors.mark_as_deleted IS NULL |sql}

          let select_sql =
            {sql|
              guardianDecodeUuid(actors.uuid),
              actors.model
            |sql}
          ;;

          let combine_sql ?(joins = "") ?(where_additions = "") select =
            Format.asprintf
              "SELECT\n  %s\nFROM %s\n  %s\nWHERE\n  %s\n  AND %s"
              select
              from_sql
              joins
              std_filter_sql
              where_additions
          ;;

          let insert_request =
            {sql|
              INSERT INTO guardian_actors (uuid, model)
              VALUES (guardianEncodeUuid(?), ?)
              ON DUPLICATE KEY UPDATE
                mark_as_deleted = NULL,
                updated_at = NOW()
            |sql}
            |> Entity.Actor.t ->. Caqti_type.unit
          ;;

          let insert ?ctx = Database.exec ?ctx insert_request %> Lwt_result.ok

          let memorize_request =
            combine_sql
              ~where_additions:{sql|actors.uuid = guardianEncodeUuid(?)|sql}
              {sql|TRUE|sql}
            |> Entity.Uuid.Actor.t ->? Caqti_type.bool
          ;;

          let mem ?ctx id =
            Database.find_opt ?ctx memorize_request id
            >|= CCOption.value ~default:false
            |> Lwt_result.ok
          ;;

          let find_request =
            combine_sql
              ~where_additions:{sql|actors.uuid = guardianEncodeUuid(?)|sql}
              select_sql
            |> Entity.(Uuid.Actor.t ->? Actor.t)
          ;;

          let find ?ctx id =
            Database.find_opt ?ctx find_request id
            >|= CCOption.to_result (not_found id)
          ;;
        end

        module Target = struct
          let not_found =
            [%show: Guard.Uuid.Target.t]
            %> Format.asprintf "Target ('%s') not found"
          ;;

          let from_sql = {sql| guardian_targets AS targets |sql}
          let std_filter_sql = {sql| targets.mark_as_deleted IS NULL |sql}

          let select_sql =
            {sql|
              guardianDecodeUuid(targets.uuid),
              targets.model
            |sql}
          ;;

          let combine_sql ?(joins = "") ?(where_additions = "") select =
            Format.asprintf
              "SELECT\n  %s\nFROM %s\n  %s\nWHERE\n  %s\n  AND %s"
              select
              from_sql
              joins
              std_filter_sql
              where_additions
          ;;

          let insert_request =
            {sql|
              INSERT INTO guardian_targets (uuid, model)
              VALUES (guardianEncodeUuid(?), ?)
              ON DUPLICATE KEY UPDATE
                mark_as_deleted = NULL,
                updated_at = NOW()
            |sql}
            |> Caqti_type.(Entity.Target.t ->. unit)
          ;;

          let insert ?ctx = Database.exec ?ctx insert_request %> Lwt_result.ok

          let memorize_request =
            combine_sql
              ~where_additions:{sql|targets.uuid = guardianEncodeUuid(?)|sql}
              {sql|TRUE|sql}
            |> Entity.Uuid.Target.t ->? Caqti_type.bool
          ;;

          let mem ?ctx id =
            Database.find_opt ?ctx memorize_request id
            >|= CCOption.value ~default:false
            |> Lwt_result.ok
          ;;

          let find_request =
            combine_sql
              ~where_additions:{sql|targets.uuid = guardianEncodeUuid(?)|sql}
              select_sql
            |> Entity.(Uuid.Target.t ->? Target.t)
          ;;

          let find ?ctx target_uuid =
            let open Lwt.Infix in
            Database.find_opt ?ctx find_request target_uuid
            >|= CCOption.to_result (not_found target_uuid)
          ;;

          let find_model_request =
            combine_sql
              ~where_additions:{sql|targets.uuid = guardianEncodeUuid(?)|sql}
              {sql|targets.model|sql}
            |> Entity.(Uuid.Target.t ->? TargetModel.t)
          ;;

          let find_model ?ctx id =
            let open Lwt.Infix in
            Database.find_opt ?ctx find_model_request id
            >|= CCOption.to_result (not_found id)
          ;;

          let promote_request =
            let open Entity in
            {sql|
              UPDATE guardian_targets
              SET model = $2, mark_as_deleted = NULL
              WHERE uuid = guardianEncodeUuid($1)
            |sql}
            |> Caqti_type.(tup2 Uuid.Target.t TargetModel.t ->. unit)
          ;;

          let promote ?ctx = CCFun.curry (Database.exec ?ctx promote_request)
        end

        let validate_model ?ctx permission model actor_uuid =
          let open Lwt.Infix in
          let validate_request =
            let open Entity in
            {sql|
              SELECT (
                SELECT TRUE
                FROM guardian_actor_roles AS roles
                JOIN guardian_role_permissions AS role_permissions
                  ON roles.role = role_permissions.role
                  AND role_permissions.mark_as_deleted IS NULL
                WHERE roles.mark_as_deleted IS NULL
                  AND roles.actor_uuid = guardianEncodeUuid($1)
                  AND role_permissions.target_model = $3
                  AND (role_permissions.permission = $2 OR role_permissions.permission = 'manage')
                LIMIT 1
              ) OR (
                SELECT TRUE
                FROM guardian_actor_permissions AS actor_permissions
                WHERE actor_permissions.mark_as_deleted IS NULL
                  AND actor_permissions.actor_uuid = guardianEncodeUuid($1)
                  AND actor_permissions.target_model = $3
                  AND (actor_permissions.permission = $2 OR actor_permissions.permission = 'manage')
                LIMIT 1
              )
            |sql}
            |> Caqti_type.(
                 tup3 Uuid.Actor.t Permission.t TargetModel.t ->? option bool)
          in
          Database.find_opt ?ctx validate_request (actor_uuid, permission, model)
          >|= CCOption.(flatten %> value ~default:false)
          >|= function
          | true -> Ok ()
          | false ->
            Error
              (Guardian.Utils.deny_message_for_str_target
                 actor_uuid
                 permission
                 ([%show: TargetModel.t] model))
        ;;

        let validate_uuid ?ctx ?model permission target_uuid actor_uuid =
          let open Lwt_result.Syntax in
          let open Lwt.Infix in
          let* model =
            model
            |> CCOption.map_or
                 ~default:(Target.find_model ?ctx target_uuid)
                 Lwt.return_ok
          in
          let validate_request =
            let open Entity in
            {sql|
              SELECT (
                SELECT TRUE
                FROM guardian_actor_roles AS roles
                  JOIN guardian_role_permissions AS role_permissions
                    ON roles.role = role_permissions.role
                    AND role_permissions.mark_as_deleted IS NULL
                  WHERE roles.mark_as_deleted IS NULL
                    AND roles.actor_uuid = guardianEncodeUuid($1)
                    AND role_permissions.target_model = $3
                    AND (role_permissions.permission = $2 OR role_permissions.permission = 'manage')
                  LIMIT 1
              ) OR (
                SELECT TRUE
                FROM guardian_actor_role_targets AS role_targets
                  JOIN guardian_role_permissions AS role_permissions
                    ON role_targets.role = role_permissions.role
                    AND role_permissions.mark_as_deleted IS NULL
                  WHERE role_targets.mark_as_deleted IS NULL
                    AND role_targets.actor_uuid = guardianEncodeUuid($1)
                    AND role_targets.target_uuid = guardianEncodeUuid($4)
                    AND role_permissions.target_model = $3
                    AND (role_permissions.permission = $2 OR role_permissions.permission = 'manage')
                    LIMIT 1
              ) OR (
                SELECT TRUE
                FROM guardian_actor_permissions AS actor_permissions
                  WHERE actor_permissions.mark_as_deleted IS NULL
                    AND actor_permissions.actor_uuid = guardianEncodeUuid($1)
                    AND (
                      (actor_permissions.target_model = $3 AND actor_permissions.target_uuid IS NULL)
                      OR
                      (actor_permissions.target_model IS NULL AND actor_permissions.target_uuid = guardianEncodeUuid($4))
                    )
                    AND (actor_permissions.permission = $2 OR actor_permissions.permission = 'manage')
                    LIMIT 1
              )
            |sql}
            |> Caqti_type.(
                 tup2
                   Uuid.Actor.t
                   (tup2 Permission.t (tup2 TargetModel.t Uuid.Target.t))
                 ->? option bool)
          in
          Database.find_opt
            ?ctx
            validate_request
            (actor_uuid, (permission, (model, target_uuid)))
          >|= CCOption.(flatten %> value ~default:false)
          >|= function
          | true -> Ok ()
          | false ->
            Error
              (Guardian.Utils.deny_message_uuid
                 actor_uuid
                 permission
                 target_uuid)
        ;;

        let validate_any_of_model ?ctx permission model actor_uuid =
          let open Lwt.Infix in
          let to_req =
            let open Entity in
            Caqti_type.(
              tup3 Uuid.Actor.t Permission.t TargetModel.t ->? option bool)
          in
          let find_bool request =
            Database.find_opt ?ctx request (actor_uuid, permission, model)
            >|= CCOption.(flatten %> value ~default:false)
          in
          let valid_or_continue fcn = function
            | true -> Lwt.return_true
            | false -> fcn
          in
          let role_permission_request =
            {sql|
              SELECT TRUE
              FROM guardian_actor_roles AS roles
              JOIN guardian_role_permissions AS role_permissions
                ON roles.role = role_permissions.role
                AND role_permissions.mark_as_deleted IS NULL
              WHERE roles.mark_as_deleted IS NULL
                AND roles.actor_uuid = guardianEncodeUuid($1)
                AND role_permissions.target_model = $3
                AND (role_permissions.permission = $2 OR role_permissions.permission = 'manage')
              LIMIT 1
            |sql}
            |> to_req
          in
          let role_permission_target_request =
            {sql|
              SELECT TRUE
              FROM guardian_actor_role_targets AS role_targets
              JOIN guardian_role_permissions AS role_permissions
                ON role_targets.role = role_permissions.role
                AND role_permissions.mark_as_deleted IS NULL
              WHERE role_targets.mark_as_deleted IS NULL
                AND role_targets.actor_uuid = guardianEncodeUuid($1)
                AND role_permissions.target_model = $3
                AND (role_permissions.permission = $2 OR role_permissions.permission = 'manage')
              LIMIT 1
            |sql}
            |> to_req
          in
          let actor_permission_request =
            {sql|
              SELECT TRUE
              FROM guardian_actor_permissions AS actor_permissions
              JOIN guardian_targets AS targets ON actor_permissions.target_uuid = targets.uuid
              WHERE actor_permissions.actor_uuid = guardianEncodeUuid($1)
                AND (actor_permissions.permission = $2 OR actor_permissions.permission = 'manage')
                AND targets.model = $3
              LIMIT 1
            |sql}
            |> to_req
          in
          find_bool role_permission_request
          >>= valid_or_continue (find_bool role_permission_target_request)
          >>= valid_or_continue (find_bool actor_permission_request)
          >|= function
          | true -> Ok ()
          | false ->
            Error
              (Guardian.Utils.deny_message_for_str_target
                 actor_uuid
                 permission
                 ([%show: TargetModel.t] model))
        ;;

        let validate
          ?ctx
          ?(any_id = false)
          ?target_uuid
          ?model
          permission
          { Guard.Actor.uuid; _ }
          =
          let open Lwt.Infix in
          (match any_id, target_uuid, model with
           | _, None, None ->
             failwith "At least a target uuid or model has to be specified!"
           | true, Some target_uuid, None ->
             Logs.warn (fun m ->
               m
                 "Validation with 'any_id' set on a 'uuid' doesn't make sense. \
                  Validating uuid.");
             validate_uuid ?ctx permission target_uuid uuid
           | true, _, Some model ->
             validate_any_of_model ?ctx permission model uuid
           | false, Some target_uuid, model ->
             validate_uuid ?ctx ?model permission target_uuid uuid
           | false, None, Some model ->
             validate_model ?ctx permission model uuid)
          >|= function
          | Ok () -> true
          | Error _ -> false
        ;;

        let define_functions ?ctx () =
          let open Caqti_type in
          [ define_encode_uuid; define_decode_uuid ]
          |> CCList.flat_map (fun (fcn_name, fcn) ->
            [ Format.asprintf {sql|DROP FUNCTION IF EXISTS %s|sql} fcn_name
            ; fcn
            ])
          |> Lwt_list.iter_s (fun query ->
            Database.exec ?ctx (query |> unit ->. unit) ())
        ;;
      end

      (** [start ?ctx ()] runs needed actions to start the backend, e.g. redefines
          needed functions **)
      let start = Repo.define_functions

      (** [find_migrations ()] returns a list of all migrations as a tuple with
          key, datetime and sql query **)
      let find_migrations () = Migrations.all

      (** [find_clean ()] returns a list of all migrations as a tuple with key and
          sql query **)
      let find_clean () =
        Migrations.all_tables
        |> CCList.map (fun m -> m, Format.asprintf "TRUNCATE TABLE %s" m)
      ;;

      (** [migrate ?ctx ()] runs all migration on a specified context [?ctx] **)
      let migrate ?ctx () =
        ()
        |> find_migrations
        |> Lwt_list.iter_s (fun (key, date, sql) ->
          Logs.debug (fun m -> m "Migration: Run '%s' from '%s'" key date);
          Database.exec ?ctx (sql |> Caqti_type.(unit ->. unit)) ())
      ;;

      (** [clean ?ctx ()] runs clean on a specified context [?ctx] **)
      let clean ?ctx () =
        ()
        |> find_clean
        |> Lwt_list.iter_s (fun (key, sql) ->
          Logs.debug (fun m -> m "Clean: Run '%s'" key);
          Database.exec ?ctx (sql |> Caqti_type.(unit ->. unit)) ())
      ;;

      let delete ?ctx () =
        Migrations.all_tables
        |> CCList.map (fun m -> m, Format.asprintf "DROP TABLE IF EXISTS %s" m)
        |> fun deletes ->
        (("skip foreign key set", "SET FOREIGN_KEY_CHECKS = 0") :: deletes)
        @ [ "add foreign key check", "SET FOREIGN_KEY_CHECKS = 0" ]
        |> Lwt_list.iter_s (fun (key, sql) ->
          Logs.debug (fun m -> m "Delete: Run '%s'" key);
          Database.exec ?ctx (sql |> Caqti_type.(unit ->. unit)) ())
      ;;
    end)
end
