open CCFun.Infix
open Lwt.Infix
open Caqti_request.Infix

let combine_lwt m =
  let%lwt k = m in
  k
;;

module Make
    (ActorModel : Guardian.RoleSig)
    (Role : Guardian.RoleSig)
    (TargetModel : Guardian.RoleSig)
    (Database : Database_pools.Sig) =
struct
  let src = Logs.Src.create "guardian.backend.mariadb"

  module Guard = Guardian.Make (ActorModel) (Role) (TargetModel)

  let lowercase_role =
    CCString.(TargetModel.show %> replace ~sub:"`" ~by:"" %> lowercase_ascii)
  ;;

  module Entity = struct
    module Uuid = struct
      let sql_select_fragment field =
        [%string
          {sql|
            LOWER(CONCAT(
              SUBSTR(HEX(%{field}), 1, 8), '-',
              SUBSTR(HEX(%{field}), 9, 4), '-',
              SUBSTR(HEX(%{field}), 13, 4), '-',
              SUBSTR(HEX(%{field}), 17, 4), '-',
              SUBSTR(HEX(%{field}), 21)
            ))
        |sql}]
      ;;

      let sql_value_fragment name =
        [%string {sql| UNHEX(REPLACE(%{name}, '-', '')) |sql}]
      ;;

      module UuidBase (Core : Guard.Uuid.Sig) = struct
        include Core

        let t =
          Caqti_type.(
            custom
              ~encode:(to_string %> CCResult.return)
              ~decode:of_string_res
              string)
        ;;
      end

      module Actor = UuidBase (Guard.Uuid.Actor)
      module Target = UuidBase (Guard.Uuid.Target)
    end

    module Role = struct
      include Role

      let t =
        Caqti_type.(
          custom
            ~encode:(Role.show %> CCResult.return)
            ~decode:of_string_res
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
            ~decode:of_string_res
            string)
      ;;
    end

    module TargetModel = struct
      include TargetModel

      let t =
        Caqti_type.(
          custom
            ~encode:(TargetModel.show %> CCResult.return)
            ~decode:of_string_res
            string)
      ;;
    end

    module Permission = struct
      include Guard.Permission

      let t =
        Caqti_type.(
          custom
            ~encode:(Guard.Permission.show %> CCResult.return)
            ~decode:of_string_res
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
          custom ~encode ~decode (t3 Uuid.Actor.t Role.t Uuid.Target.t))
      ;;

      let role =
        let encode { actor_uuid; role; target_uuid } =
          match target_uuid with
          | Some _ -> Error "target_uuid defined for role only model"
          | None -> Ok (actor_uuid, role)
        in
        let decode (actor_uuid, role) =
          Ok { actor_uuid; role; target_uuid = None }
        in
        Caqti_type.(custom ~encode ~decode (t2 Uuid.Actor.t Role.t))
      ;;

      let t =
        let encode _ = Error "Read only model of ActorRoles" in
        let decode (actor_uuid, role, target_uuid) =
          Ok { actor_uuid; role; target_uuid }
        in
        Caqti_type.(
          custom ~encode ~decode (t3 Uuid.Actor.t Role.t (option Uuid.Target.t)))
      ;;
    end

    module Actor = struct
      include Guard.Actor

      let t =
        let encode m = Ok (m.uuid, m.model) in
        let decode (uuid, model) = Ok { uuid; model } in
        Caqti_type.(custom ~encode ~decode (t2 Uuid.Actor.t ActorModel.t))
      ;;
    end

    module Target = struct
      include Guard.Target

      let t =
        let encode m = Ok (m.uuid, m.model) in
        let decode (uuid, model) = Ok { uuid; model } in
        Caqti_type.(custom ~encode ~decode (t2 Uuid.Target.t TargetModel.t))
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
            (t2 (option TargetModel.t) (option Uuid.Target.t)))
      ;;
    end

    module RolePermission = struct
      include Guard.RolePermission

      let t =
        let encode m = Ok (m.role, m.permission, m.model) in
        let decode (role, permission, model) = Ok { role; permission; model } in
        Caqti_type.(
          custom ~encode ~decode (t3 Role.t Permission.t TargetModel.t))
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
          custom ~encode ~decode (t3 Uuid.Actor.t Permission.t TargetEntity.t))
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
            (t3 Permission.t TargetModel.t (option Uuid.Target.t)))
      ;;
    end

    module RoleAssignment = struct
      include Guard.RoleAssignment

      let t =
        let open Caqti_encoders in
        let decode (role, (target_role, ())) = Ok { role; target_role } in
        let encode m : ('a Caqti_encoders.Data.t, string) result =
          Ok Data.[ m.role; m.target_role ]
        in
        custom ~encode ~decode Schema.[ Role.t; Role.t ]
      ;;
    end
  end

  module DBCache = struct
    let pool_of_ctx =
      CCOption.flat_map (CCList.assoc_opt ~eq:CCString.equal "pool")
    ;;

    (* Flat LRU cache keyed by the full (actor, pool, any_id, permission,
       target_uuid, model) tuple plus a global epoch and a per-actor
       generation number.  A total capacity cap means entries are evicted in
       LRU order rather than growing without bound. *)

    type cache_key =
      { epoch : int
      ; actor : string
      ; generation : int
      ; pool : string option
      ; any_id : bool
      ; permission : Guard.Permission.t
      ; target_uuid : Guard.Uuid.Target.t option
      ; model : TargetModel.t option
      }

    module CacheKey = struct
      type t = cache_key

      let equal a b =
        CCInt.equal a.epoch b.epoch
        && CCString.equal a.actor b.actor
        && CCInt.equal a.generation b.generation
        && CCOption.equal CCString.equal a.pool b.pool
        && CCBool.equal a.any_id b.any_id
        && Guard.Permission.equal a.permission b.permission
        && CCOption.equal Guard.Uuid.Target.equal a.target_uuid b.target_uuid
        && CCOption.equal TargetModel.equal a.model b.model
      ;;

      let hash = CCHash.poly
    end

    module CacheValue = struct
      type t = bool

      let weight _ = 1
    end

    module LruCache = Lru.M.Make (CacheKey) (CacheValue)

    (* Total number of (actor, permission, target) entries kept across all
       actors.  Evicts the least-recently-used entry when the limit is hit. *)
    let capacity = 4096
    let _cache = ref (LruCache.create capacity)

    (* [epoch] scopes every key at once: bumping it makes all previously
       stored keys unreachable.  [generations] scopes a single actor's keys.
       Both are snapshotted into the key by [make_key] and therefore captured
       when a validation *starts*; if an invalidation races an in-flight
       validation, the result is stored under the (now stale) snapshot and is
       unreachable to later lookups instead of overwriting the invalidation.
       [max_tracked_actors] keeps the generations table from growing with
       every actor ever invalidated. *)
    let epoch = ref 0

    module Generations = CCHashtbl.Make (CCString)

    let generations : int Generations.t = Generations.create 256
    let max_tracked_actors = 100_000

    (* Bumping [epoch] invalidates every key (including snapshots taken by
       in-flight validations), so resetting [generations] back to 0 here is
       safe: no surviving entry can share the new epoch. *)
    let clear () =
      incr epoch;
      _cache := LruCache.create capacity;
      Generations.reset generations
    ;;

    let generation actor = Generations.get_or generations actor ~default:0

    (** Remove all cached entries for a single actor. Used when that actor's
        roles or direct permissions change. *)
    let clear_actor uuid =
      let actor = Guard.Uuid.Actor.to_string uuid in
      if
        Generations.length generations >= max_tracked_actors
        && not (Generations.mem generations actor)
      then clear ()
      else Generations.incr generations actor
    ;;

    let make_key ctx any_id actor_uuid permission target_uuid model =
      let actor = Guard.Uuid.Actor.to_string actor_uuid in
      { epoch = !epoch
      ; actor
      ; generation = generation actor
      ; pool = pool_of_ctx ctx
      ; any_id
      ; permission
      ; target_uuid
      ; model
      }
    ;;

    (* [find] and [store] take a key precomputed by [make_key] so a single
       validation snapshots the epoch/generation once and reuses it for both
       the lookup and the store. *)
    let find key =
      match LruCache.find key !_cache with
      | Some _ as v ->
        LruCache.promote key !_cache;
        v
      | None -> None
    ;;

    let store key result =
      LruCache.add key result !_cache;
      LruCache.trim !_cache
    ;;
  end

  (* Caches the target uuid -> model resolution used by [validate], so a
     validation-cache hit does not still cost a database round trip.
     Invalidated by [Target.promote] and by [clear_cache]/[clean]/[delete]. *)
  module ModelCache = struct
    type model_key =
      { epoch : int
      ; pool : string option
      ; target : string
      }

    module CacheKey = struct
      type t = model_key

      let equal a b =
        let open CCString in
        CCInt.equal a.epoch b.epoch
        && CCOption.equal equal a.pool b.pool
        && equal a.target b.target
      ;;

      let hash = CCHash.poly
    end

    module CacheValue = struct
      type t = TargetModel.t

      let weight _ = 1
    end

    module LruCache = Lru.M.Make (CacheKey) (CacheValue)

    let capacity = 4096
    let _cache = ref (LruCache.create capacity)

    (* [epoch] is snapshotted into the key when a resolution *starts*.
       Bumping it (via [invalidate]/[clear]) makes every earlier key
       unreachable, so a [promote] that races an in-flight resolution cannot
       be overwritten by the stale model the resolution was still computing;
       the stale entry ages out via LRU instead. *)
    let epoch = ref 0
    let clear () = incr epoch

    let make_key ctx uuid =
      { epoch = !epoch
      ; pool = DBCache.pool_of_ctx ctx
      ; target = Guard.Uuid.Target.to_string uuid
      }
    ;;

    let find key =
      match LruCache.find key !_cache with
      | Some _ as v ->
        LruCache.promote key !_cache;
        v
      | None -> None
    ;;

    let store key model =
      LruCache.add key model !_cache;
      LruCache.trim !_cache
    ;;

    (* A promoted target changes model, so drop its cached resolution and bump
       the epoch to also discard any in-flight resolution of the old model. *)
    let invalidate ctx uuid =
      LruCache.remove (make_key ctx uuid) !_cache;
      incr epoch
    ;;
  end

  include Guard.MakePersistence (struct
      type actor = Guard.Actor.t
      type actor_model = ActorModel.t
      type actor_permission = Guard.ActorPermission.t
      type actor_role = Guard.ActorRole.t
      type permission_on_target = Guard.PermissionOnTarget.t
      type role = Role.t
      type role_assignment = Guard.RoleAssignment.t
      type role_permission = Guard.RolePermission.t
      type target = Guard.Target.t
      type target_entity = Guard.TargetEntity.t
      type target_model = TargetModel.t
      type validation_set = Guard.ValidationSet.t

      module Repo = struct
        let clear_cache () =
          DBCache.clear ();
          ModelCache.clear ()
        ;;

        module Model = struct
          let role = Entity.Role.t
          let role_assignment = Entity.RoleAssignment.t
        end

        let combine_sql
              from_sql
              std_filter_sql
              ?(joins = "")
              ?where_additions
              select
          =
          Format.asprintf
            "SELECT\n  %s\nFROM  %s\n  %s\nWHERE\n  %s\n  %s"
            select
            from_sql
            joins
            std_filter_sql
            (CCOption.map_or
               ~default:""
               (Format.asprintf "AND %s")
               where_additions)
        ;;

        module ActorRole = struct
          let upsert_uuid_request =
            let open Entity.Uuid in
            [%string
              {sql|
                INSERT INTO guardian_actor_role_targets (actor_uuid, role, target_uuid)
                VALUES (
                  %{sql_value_fragment "?"},
                  ?,
                  %{sql_value_fragment "?"}
                )
                ON DUPLICATE KEY UPDATE
                  mark_as_deleted = NULL,
                  updated_at = NOW()
              |sql}]
            |> Entity.ActorRole.targets ->. Caqti_type.unit
          ;;

          let upsert_model_request =
            [%string
              {sql|
                INSERT INTO guardian_actor_roles (actor_uuid, role)
                VALUES (%{Entity.Uuid.sql_value_fragment "?"}, ?)
                ON DUPLICATE KEY UPDATE
                  mark_as_deleted = NULL,
                  updated_at = NOW()
              |sql}]
            |> Entity.ActorRole.role ->. Caqti_type.unit
          ;;

          let upsert
                ?ctx
                ({ Entity.ActorRole.target_uuid; actor_uuid; _ } as role)
            =
            let () = DBCache.clear_actor actor_uuid in
            match target_uuid with
            | Some _ -> Database.exec ?ctx upsert_uuid_request role
            | None -> Database.exec ?ctx upsert_model_request role
          ;;

          let find_by_actor_request =
            let open Entity.Uuid in
            [%string
              {sql|
                SELECT
                  %{sql_select_fragment "role_targets.actor_uuid"},
                  role_targets.role,
                  %{sql_select_fragment "role_targets.target_uuid"}
                FROM guardian_actor_role_targets AS role_targets
                WHERE role_targets.actor_uuid = %{sql_value_fragment "$1"}
                  AND role_targets.mark_as_deleted IS NULL
                UNION ALL
                SELECT %{sql_select_fragment "roles.actor_uuid"}, roles.role, NULL
                FROM guardian_actor_roles AS roles
                WHERE roles.actor_uuid = %{sql_value_fragment "$1"}
                  AND roles.mark_as_deleted IS NULL
              |sql}]
            |> Entity.(Uuid.Actor.t ->* ActorRole.t)
          ;;

          let find_by_actor ?ctx = Database.collect ?ctx find_by_actor_request

          let find_by_target_request =
            let open Entity.Uuid in
            [%string
              {sql|
                SELECT
                  %{sql_select_fragment "role_targets.actor_uuid"},
                  role_targets.role,
                  %{sql_select_fragment "role_targets.target_uuid"}
                FROM guardian_actor_role_targets AS role_targets
                WHERE role_targets.role = $1
                  AND role_targets.target_uuid = %{Entity.Uuid.sql_value_fragment "$2"}
                  AND role_targets.mark_as_deleted IS NULL
                UNION ALL
                SELECT %{sql_select_fragment "roles.actor_uuid"}, roles.role, NULL
                FROM guardian_actor_roles AS roles
                WHERE roles.role = $1
                  AND roles.mark_as_deleted IS NULL
              |sql}]
            |> Entity.(Caqti_type.t2 Role.t Uuid.Target.t ->* ActorRole.t)
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
                       , dyn |> add Model.role role )
                     | None ->
                       ( "exclude.role = ? AND exclude.target_uuid IS NULL"
                         :: args
                       , dyn |> add Model.role role )
                     | Some uuid ->
                       ( [%string
                           {sql|(exclude.role = ? AND exclude.target_uuid = %{Entity.Uuid.sql_value_fragment "?"})|sql}]
                         :: args
                       , dyn
                         |> add Model.role role
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
            [%string
              {sql|
                SELECT %{Entity.Uuid.sql_select_fragment "roles.actor_uuid"}
                FROM guardian_actor_roles AS roles
                WHERE roles.role = ?
                  AND roles.mark_as_deleted IS NULL
                  %{exclude_sql}
              |sql}]
            |> params ->* Entity.Uuid.Actor.t
          ;;

          let find_actors_by_target_request ?(exclude_sql = "") params =
            [%string
              {sql|
                SELECT %{Entity.Uuid.sql_select_fragment "role_targets.actor_uuid"}
                FROM guardian_actor_role_targets AS role_targets
                WHERE role_targets.target_uuid = %{Entity.Uuid.sql_value_fragment "?"}
                  AND role_targets.mark_as_deleted IS NULL
                  AND role_targets.role = ?
                  %{exclude_sql}
              |sql}]
            |> params ->* Entity.Uuid.Actor.t
          ;;

          let find_actors_by_role ?ctx ?(exclude = []) (role, target_uuid) =
            let open Guardian.Utils.Dynparam in
            match target_uuid with
            | Some uuid ->
              let field = "role_targets.actor_uuid" in
              let dynparam =
                empty |> add Entity.Uuid.Target.t uuid |> add Model.role role
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
              let dynparam = empty |> add Model.role role in
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
            [%string
              {sql|
                SELECT
                  role_permissions.permission,
                  role_permissions.target_model,
                  %{Uuid.sql_select_fragment "roles.target_uuid"}
                FROM
                  guardian_actor_role_targets AS roles
                JOIN guardian_role_permissions AS role_permissions
                  ON role_permissions.role = roles.role
                  AND role_permissions.mark_as_deleted IS NULL
                WHERE
                  roles.mark_as_deleted IS NULL
                  AND roles.actor_uuid = %{Uuid.sql_value_fragment "$1"}
                UNION
                SELECT
                  role_permissions.permission,
                  role_permissions.target_model,
                  NULL
                FROM
                  guardian_actor_roles AS roles
                JOIN guardian_role_permissions AS role_permissions
                  ON role_permissions.role = roles.role
                  AND role_permissions.mark_as_deleted IS NULL
                WHERE
                  roles.mark_as_deleted IS NULL
                  AND roles.actor_uuid = %{Uuid.sql_value_fragment "$1"}
                UNION
                SELECT
                  actor_permissions.permission,
                  COALESCE (actor_permissions.target_model, targets.model),
                  %{Uuid.sql_select_fragment "actor_permissions.target_uuid"}
                FROM
                  guardian_actor_permissions AS actor_permissions
                LEFT JOIN guardian_targets AS targets
                  ON targets.uuid = actor_permissions.target_uuid
                  AND targets.mark_as_deleted IS NULL
                WHERE
                  actor_permissions.actor_uuid = %{Uuid.sql_value_fragment "$1"}
                  AND actor_permissions.mark_as_deleted IS NULL
              |sql}]
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
            [%string
              {sql|
                UPDATE guardian_actor_role_targets
                SET mark_as_deleted = NOW()
                WHERE actor_uuid = %{Entity.Uuid.sql_value_fragment "$1"}
                  AND role = $2
                  AND target_uuid = %{Entity.Uuid.sql_value_fragment "$3"}
              |sql}]
            |> Entity.ActorRole.targets ->. Caqti_type.unit
          ;;

          let delete_role_model_request =
            let open Entity in
            [%string
              {sql|
                UPDATE guardian_actor_roles
                SET mark_as_deleted = NOW()
                WHERE actor_uuid = %{Uuid.sql_value_fragment "$1"}
                  AND role = $2
              |sql}]
            |> Caqti_type.(t2 Uuid.Actor.t Model.role ->. unit)
          ;;

          let delete ?ctx role =
            let open Guard.ActorRole in
            let () = DBCache.clear_actor role.actor_uuid in
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

          let combine_sql = combine_sql from_sql std_filter_sql

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

          let insert ?ctx rp =
            (* A changed rule affects every actor holding the role; dropping
               the bounded cache outright is cheaper than enumerating those
               actors. *)
            let () = DBCache.clear () in
            Database.exec ?ctx insert_request rp |> Lwt_result.ok
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

          let delete ?ctx rp =
            (* See [insert]: invalidate all actors at once. *)
            let () = DBCache.clear () in
            Database.exec ?ctx delete_request rp |> Lwt_result.ok
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
            Entity.Uuid.
              [ sql_select_fragment "actor_permissions.actor_uuid"
              ; "actor_permissions.permission"
              ; "actor_permissions.target_model"
              ; sql_select_fragment "actor_permissions.target_uuid"
              ]
            |> CCString.concat ",\n"
          ;;

          let combine_sql = combine_sql from_sql std_filter_sql

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
              [%string
                {sql|actor_permissions.target_uuid = %{Entity.Uuid.sql_value_fragment "$1"}
                  OR actor_permissions.target_model = (SELECT targets.model FROM guardian_targets AS targets WHERE targets.uuid = %{Entity.Uuid.sql_value_fragment "$1"})
                |sql}]
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
            [%string
              {sql|
                INSERT INTO guardian_actor_permissions (actor_uuid, permission, target_model, target_uuid)
                VALUES (%{Entity.Uuid.sql_value_fragment "?"}, ?, ?, %{Entity.Uuid.sql_value_fragment "?"})
                ON DUPLICATE KEY UPDATE
                  mark_as_deleted = NULL,
                  updated_at = NOW()
              |sql}]
            |> Entity.ActorPermission.t ->. Caqti_type.unit
          ;;

          let insert ?ctx ({ Guard.ActorPermission.actor_uuid; _ } as ap) =
            DBCache.clear_actor actor_uuid;
            Database.exec ?ctx insert_request ap |> Lwt_result.ok
          ;;

          let delete_request =
            let open Entity.Uuid in
            [%string
              {sql|
                UPDATE guardian_actor_permissions
                SET mark_as_deleted = NOW()
                WHERE actor_uuid = %{sql_value_fragment "$1"}
                  AND permission = $2
                  AND (($3 IS NULL AND target_model IS NULL) OR target_model = $3)
                  AND (($4 IS NULL AND target_uuid IS NULL) OR target_uuid = %{sql_value_fragment "$4"})
              |sql}]
            |> Entity.ActorPermission.t ->. Caqti_type.unit
          ;;

          let delete
                ?ctx
                ({ Guard.ActorPermission.actor_uuid; _ } as permission)
            =
            DBCache.clear_actor actor_uuid;
            Database.exec ?ctx delete_request permission |> Lwt_result.ok
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
            [ Entity.Uuid.sql_select_fragment "actors.uuid"; "actors.model" ]
            |> CCString.concat ",\n"
          ;;

          let combine_sql = combine_sql from_sql std_filter_sql

          let insert_request =
            [%string
              {sql|
                INSERT INTO guardian_actors (uuid, model)
                VALUES (%{Entity.Uuid.sql_value_fragment "?"}, ?)
                ON DUPLICATE KEY UPDATE
                  mark_as_deleted = NULL,
                  updated_at = NOW()
              |sql}]
            |> Entity.Actor.t ->. Caqti_type.unit
          ;;

          let insert ?ctx = Database.exec ?ctx insert_request %> Lwt_result.ok

          let memorize_request =
            combine_sql
              ~where_additions:
                [%string
                  {sql|actors.uuid = %{Entity.Uuid.sql_value_fragment "?"}|sql}]
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
              ~where_additions:
                [%string
                  {sql|actors.uuid = %{Entity.Uuid.sql_value_fragment "?"}|sql}]
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
            [ Entity.Uuid.sql_select_fragment "targets.uuid"; "targets.model" ]
            |> CCString.concat ",\n"
          ;;

          let combine_sql = combine_sql from_sql std_filter_sql

          let insert_request =
            [%string
              {sql|
                INSERT INTO guardian_targets (uuid, model)
                VALUES (%{Entity.Uuid.sql_value_fragment "?"}, ?)
                ON DUPLICATE KEY UPDATE
                  mark_as_deleted = NULL,
                  updated_at = NOW()
              |sql}]
            |> Caqti_type.(Entity.Target.t ->. unit)
          ;;

          let insert ?ctx = Database.exec ?ctx insert_request %> Lwt_result.ok

          let memorize_request =
            combine_sql
              ~where_additions:
                [%string
                  {sql|targets.uuid = %{Entity.Uuid.sql_value_fragment "?"}|sql}]
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
              ~where_additions:
                [%string
                  {sql|targets.uuid = %{Entity.Uuid.sql_value_fragment "?"}|sql}]
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
              ~where_additions:
                [%string
                  {sql|targets.uuid = %{Entity.Uuid.sql_value_fragment "?"}|sql}]
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
            [%string
              {sql|
                UPDATE guardian_targets
                SET model = $2, mark_as_deleted = NULL
                WHERE uuid = %{Uuid.sql_value_fragment "$1"}
              |sql}]
            |> Caqti_type.(t2 Uuid.Target.t TargetModel.t ->. unit)
          ;;

          let promote ?ctx uuid model =
            let () = ModelCache.invalidate ctx uuid in
            Database.exec ?ctx promote_request (uuid, model)
          ;;
        end

        module RoleAssignment = struct
          let table_name = "guardian_assign_roles"
          let sql_insert_columns = [ "role"; "target_role" ]

          let sql_select_columns =
            [ "guardian_assign_roles.role"
            ; "guardian_assign_roles.target_role"
            ]
          ;;

          let find_request_sql =
            Mariadb_utils.find_request_sql
              sql_select_columns
              table_name
              ~default_where:None
              ~joins:""
          ;;

          let insert ?ctx =
            Database.populate
              ?ctx
              table_name
              sql_insert_columns
              Model.role_assignment
          ;;

          let find_all_request =
            find_request_sql "" |> Caqti_type.(unit ->* Model.role_assignment)
          ;;

          let find_all ?ctx = Database.collect ?ctx find_all_request

          let find_all_by_role_request =
            find_request_sql {sql|WHERE role = ?|sql}
            |> Model.(role ->* role_assignment)
          ;;

          let find_all_by_role ?ctx =
            Database.collect ?ctx find_all_by_role_request
          ;;

          let delete_add_history_request =
            {sql|
              INSERT INTO guardian_assign_roles_history (role, target_role, comment) VALUES (?,?,?)
            |sql}
            |> Caqti_type.(t2 Model.role_assignment (option string) ->. unit)
          ;;

          let delete_remove_request =
            {sql|
                DELETE FROM guardian_assign_roles WHERE role = ? AND target_role = ?
            |sql}
            |> Model.role_assignment ->. Caqti_type.unit
          ;;

          let delete ?ctx ?comment role =
            let with_connection request input connection =
              let (module Connection : Caqti_lwt.CONNECTION) = connection in
              Connection.exec request input
            in
            Database.transaction_iter
              ?ctx
              [ with_connection delete_add_history_request (role, comment)
              ; with_connection delete_remove_request role
              ]
          ;;
        end

        let validate_model ?ctx permission model actor_uuid =
          let open Lwt.Infix in
          let validate_request =
            let open Entity in
            [%string
              {sql|
                SELECT (
                  SELECT TRUE
                  FROM guardian_actor_roles AS roles
                  JOIN guardian_role_permissions AS role_permissions
                    ON roles.role = role_permissions.role
                    AND role_permissions.mark_as_deleted IS NULL
                  WHERE roles.mark_as_deleted IS NULL
                    AND roles.actor_uuid = %{Uuid.sql_value_fragment "$1"}
                    AND role_permissions.target_model = $3
                    AND (role_permissions.permission = $2 OR role_permissions.permission = 'manage')
                  LIMIT 1
                ) OR (
                  SELECT TRUE
                  FROM guardian_actor_permissions AS actor_permissions
                  WHERE actor_permissions.mark_as_deleted IS NULL
                    AND actor_permissions.actor_uuid = %{Uuid.sql_value_fragment "$1"}
                    AND actor_permissions.target_model = $3
                    AND (actor_permissions.permission = $2 OR actor_permissions.permission = 'manage')
                  LIMIT 1
                )
              |sql}]
            |> Caqti_type.(
                 t3 Uuid.Actor.t Permission.t TargetModel.t ->? option bool)
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
            [%string
              {sql|
                SELECT (
                  SELECT TRUE
                  FROM guardian_actor_roles AS roles
                    JOIN guardian_role_permissions AS role_permissions
                      ON roles.role = role_permissions.role
                      AND role_permissions.mark_as_deleted IS NULL
                    WHERE roles.mark_as_deleted IS NULL
                      AND roles.actor_uuid = %{Uuid.sql_value_fragment "$1"}
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
                      AND role_targets.actor_uuid = %{Uuid.sql_value_fragment "$1"}
                      AND role_targets.target_uuid = %{Uuid.sql_value_fragment "$4"}
                      AND role_permissions.target_model = $3
                      AND (role_permissions.permission = $2 OR role_permissions.permission = 'manage')
                      LIMIT 1
                ) OR (
                  SELECT TRUE
                  FROM guardian_actor_permissions AS actor_permissions
                    WHERE actor_permissions.mark_as_deleted IS NULL
                      AND actor_permissions.actor_uuid = %{Uuid.sql_value_fragment "$1"}
                      AND (
                        (actor_permissions.target_model = $3 AND actor_permissions.target_uuid IS NULL)
                        OR
                        (actor_permissions.target_model IS NULL AND actor_permissions.target_uuid = %{Uuid.sql_value_fragment "$4"})
                      )
                      AND (actor_permissions.permission = $2 OR actor_permissions.permission = 'manage')
                      LIMIT 1
                )
              |sql}]
            |> Caqti_type.(
                 t2
                   Uuid.Actor.t
                   (t2 Permission.t (t2 TargetModel.t Uuid.Target.t))
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
          let validate_request =
            let open Entity in
            [%string
              {sql|
                SELECT (
                  SELECT TRUE
                  FROM guardian_actor_roles AS roles
                  JOIN guardian_role_permissions AS role_permissions
                    ON roles.role = role_permissions.role
                    AND role_permissions.mark_as_deleted IS NULL
                  WHERE roles.mark_as_deleted IS NULL
                    AND roles.actor_uuid = %{Uuid.sql_value_fragment "$1"}
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
                    AND role_targets.actor_uuid = %{Uuid.sql_value_fragment "$1"}
                    AND role_permissions.target_model = $3
                    AND (role_permissions.permission = $2 OR role_permissions.permission = 'manage')
                  LIMIT 1
                ) OR (
                  SELECT TRUE
                  FROM guardian_actor_permissions AS actor_permissions
                  LEFT JOIN guardian_targets AS targets
                    ON actor_permissions.target_uuid = targets.uuid
                    AND targets.mark_as_deleted IS NULL
                  WHERE actor_permissions.mark_as_deleted IS NULL
                    AND actor_permissions.actor_uuid = %{Uuid.sql_value_fragment "$1"}
                    AND (actor_permissions.permission = $2 OR actor_permissions.permission = 'manage')
                    AND (targets.model = $3 OR actor_permissions.target_model = $3)
                  LIMIT 1
                )
              |sql}]
            |> Caqti_type.(
                 t3 Uuid.Actor.t Permission.t TargetModel.t ->? option bool)
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

        let validate
              ?ctx
              ?(any_id = false)
              ?target_uuid
              ?model
              permission
              { Guard.Actor.uuid; _ }
          =
          let open Lwt.Infix in
          let log_result granted =
            let level, status =
              if granted then Logs.Debug, "granted" else Logs.Info, "denied"
            in
            Logs.msg ~src level (fun m ->
              let target =
                match target_uuid, model with
                | Some t, _ -> Guard.Uuid.Target.to_string t
                | None, Some mdl -> [%show: TargetModel.t] mdl
                | None, None -> "none"
              in
              m
                "Access %s: actor=%s permission=%s target=%s"
                status
                (Guard.Uuid.Actor.to_string uuid)
                (Guard.Permission.show permission)
                target)
          in
          (* [run_and_cache cache_model query] checks the cache keyed by
             [cache_model], runs [query] on a miss, then stores the result
             under the same key.  Callers must pass the fully-resolved model
             so the key is stable and concrete.  The key is snapshotted once,
             before running [query], so an invalidation that races the query
             cannot be overwritten by this (now stale) result. *)
          let run_and_cache cache_model query =
            let key =
              DBCache.make_key
                ctx
                any_id
                uuid
                permission
                target_uuid
                cache_model
            in
            match DBCache.find key with
            | Some granted ->
              log_result granted;
              Lwt.return granted
            | None ->
              query
              >|= fun result ->
              let granted = CCResult.is_ok result in
              log_result granted;
              let () = DBCache.store key granted in
              granted
          in
          match any_id, target_uuid, model with
          | _, None, None ->
            run_and_cache
              None
              (Lwt.return_error
                 "At least a target uuid or model has to be specified!")
          | true, Some target_uuid, None ->
            Logs.warn ~src (fun m ->
              m
                "Validation with 'any_id' set on a 'uuid' doesn't make sense. \
                 Validating uuid.");
            run_and_cache None (validate_uuid ?ctx permission target_uuid uuid)
          | true, _, Some mdl ->
            run_and_cache model (validate_any_of_model ?ctx permission mdl uuid)
          | false, Some target_uuid, None ->
            (* Resolve the model up front so both the DB query and the cache
               key use the concrete model, preventing stale model=None entries
               if the target's model later changes (e.g. via Target.promote).
               The resolution itself is cached so a validation-cache hit does
               not still cost a database round trip.  The model-cache key is
               snapshotted before the resolution query so a racing promote is
               not overwritten by the stale model being resolved here. *)
            let model_key = ModelCache.make_key ctx target_uuid in
            (match ModelCache.find model_key with
             | Some model -> Lwt.return_ok model
             | None ->
               Target.find_model ?ctx target_uuid
               >|= CCResult.map (fun model ->
                 ModelCache.store model_key model;
                 model))
            >>= (function
             | Error _ as e ->
               let granted = CCResult.is_ok e in
               log_result granted;
               Lwt.return granted
             | Ok resolved_model ->
               run_and_cache
                 (Some resolved_model)
                 (validate_uuid
                    ?ctx
                    ~model:resolved_model
                    permission
                    target_uuid
                    uuid))
          | false, Some target_uuid, Some mdl ->
            run_and_cache
              model
              (validate_uuid ?ctx ~model:mdl permission target_uuid uuid)
          | false, None, Some mdl ->
            run_and_cache model (validate_model ?ctx permission mdl uuid)
        ;;
      end

      (** [find_migrations ()] returns a list of all migrations as a tuple with
          key, datetime and sql query **)
      let find_migrations () = Migrations.all

      (** [find_clean ()] returns a list of all migrations as a tuple with key and
          sql query **)
      let find_clean () =
        Migrations.all_tables
        |> CCList.map (fun m -> m, [%string "TRUNCATE TABLE %{m}"])
      ;;

      (** [migrate ?ctx ()] runs all migration on a specified context [?ctx] **)
      let migrate ?ctx () =
        ()
        |> find_migrations
        |> Lwt_list.iter_s (fun (key, date, sql) ->
          Logs.debug ~src (fun m -> m "Migration: Run '%s' from '%s'" key date);
          Database.exec ?ctx (sql |> Caqti_type.(unit ->. unit)) ())
      ;;

      let run_without_fk_checks ?ctx label stmts =
        (("disable foreign key checks", "SET FOREIGN_KEY_CHECKS = 0") :: stmts)
        @ [ "enable foreign key checks", "SET FOREIGN_KEY_CHECKS = 1" ]
        |> Lwt_list.iter_s (fun (key, sql) ->
          Logs.debug ~src (fun m -> m "%s: Run '%s'" label key);
          Database.exec ?ctx (sql |> Caqti_type.(unit ->. unit)) ())
      ;;

      (** [clean ?ctx ()] runs clean on a specified context [?ctx] **)
      let clean ?ctx () =
        let () = Repo.clear_cache () in
        find_clean () |> run_without_fk_checks ?ctx "Clean"
      ;;

      let delete ?ctx () =
        let () = Repo.clear_cache () in
        Migrations.all_tables
        |> CCList.map (fun m -> m, Format.asprintf "DROP TABLE IF EXISTS %s" m)
        |> run_without_fk_checks ?ctx "Delete"
      ;;
    end)
end
