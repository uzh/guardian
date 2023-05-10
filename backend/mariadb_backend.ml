open CCFun
open Lwt.Infix
open Caqti_request.Infix

module Make
  (ActorRoles : Guardian.RoleSig)
  (TargetRoles : Guardian.RoleSig)
  (Database : Database_pools.Sig) =
struct
  module Guard = Guardian.Make (ActorRoles) (TargetRoles)
  module Authorizer = Guard.Authorizer

  let lowercase_role =
    CCString.(TargetRoles.show %> replace ~sub:"`" ~by:"" %> lowercase_ascii)
  ;;

  let capitalize_role =
    CCString.(TargetRoles.show %> replace ~sub:"`" ~by:"" %> capitalize_ascii)
  ;;

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

  module Owner = struct
    let t = Uuid.Actor.t
  end

  module Role = struct
    include ActorRoles

    let t =
      let open CCResult in
      Caqti_type.(
        custom
          ~encode:(ActorRoles.show %> return)
          ~decode:(of_string %> return)
          string)
    ;;

    let roles =
      let open CCResult in
      Caqti_type.(
        custom
          ~encode:
            (CCList.map (ActorRoles.show %> Format.asprintf "'%s'")
             %> CCString.concat ", "
             %> return)
          ~decode:(Error "Retreive a list of roles isn't supported" |> const)
          string)
    ;;
  end

  module Query = struct
    include Guard.Relation.Query

    let t =
      let open CCResult in
      Caqti_type.(
        custom
          ~encode:(to_string %> return)
          ~decode:(of_string %> return)
          string)
    ;;
  end

  module Kind = struct
    include TargetRoles

    let t =
      let open CCResult in
      Caqti_type.(
        custom
          ~encode:(TargetRoles.show %> return)
          ~decode:(of_string %> return)
          string)
    ;;
  end

  module Action = struct
    include Guard.Action

    let t =
      let open CCResult in
      Caqti_type.(
        custom
          ~encode:(Guard.Action.show %> return)
          ~decode:(of_string %> return)
          string)
    ;;
  end

  include Guard.MakePersistence (struct
    type 'a actor = 'a Guard.Actor.t
    type 'b target = 'b Guard.Target.t
    type actor_spec = Guard.ActorSpec.t
    type effect = Guard.Effect.t
    type validation_set = Guard.ValidationSet.t
    type kind = TargetRoles.t
    type query = Guard.Relation.Query.t
    type relation = Guard.Relation.t
    type role_set = Guard.RoleSet.t
    type roles = ActorRoles.t
    type rule = Guard.Rule.t
    type target_spec = Guard.TargetSpec.t
    type ('rv, 'err) monad = ('rv, 'err) Lwt_result.t

    module Repo = struct
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

      module Roles = struct
        let upsert_request =
          {sql|
            INSERT INTO guardian_actor_roles (actor_uuid, role)
            VALUES (guardianEncodeUuid(?), ?)
            ON DUPLICATE KEY UPDATE
                updated_at = NOW()
          |sql}
          |> Caqti_type.(tup2 Uuid.Actor.t Role.t ->. unit)
        ;;

        let upsert ?ctx = Database.exec ?ctx upsert_request

        let find_by_actor_request =
          {sql|
            SELECT role
            FROM guardian_actor_roles AS roles
            WHERE roles.actor_uuid = guardianEncodeUuid(?)
          |sql}
          |> Uuid.Actor.t ->* Role.t
        ;;

        let find_by_actor ?ctx =
          Database.collect ?ctx find_by_actor_request
          %> Lwt.map Guard.RoleSet.of_list
        ;;

        let find_actors_by_role_request =
          {sql|
            SELECT guardianDecodeUuid(roles.actor_uuid)
            FROM guardian_actor_roles AS roles
            WHERE roles.role = ?
          |sql}
          |> Role.t ->* Uuid.Actor.t
        ;;

        let find_actors_by_role ?ctx =
          Database.collect ?ctx find_actors_by_role_request
        ;;

        let find_actors_by_roles_request =
          {sql|
            SELECT roles.role, guardianDecodeUuid(roles.actor_uuid)
            FROM guardian_actor_roles AS roles
            WHERE roles.role IN (?)
          |sql}
          |> Role.roles ->* Caqti_type.(tup2 Role.t Uuid.Actor.t)
        ;;

        let find_actors_by_roles ?ctx roles =
          let%lwt find_actors =
            Database.collect ?ctx find_actors_by_roles_request roles
          in
          CCList.map
            (fun role ->
              let actors =
                CCList.filter_map
                  (fun (arole, id) ->
                    if Role.equal role arole then Some id else None)
                  find_actors
              in
              role, actors)
            roles
          |> Lwt.return
        ;;

        let delete_request =
          {sql|
            DELETE FROM guardian_actor_roles
            WHERE actor_uuid = guardianEncodeUuid(?) AND role = ?
          |sql}
          |> Caqti_type.(tup2 Uuid.Actor.t Role.t ->. unit)
        ;;

        let delete ?ctx = Database.exec ?ctx delete_request
      end

      module Rule = struct
        include Guard.Rule

        let t =
          let encode =
            let open Guard in
            function
            | ActorSpec.Entity arole, act, TargetSpec.Entity trole ->
              Ok (arole, (None, (act, (trole, None))))
            | ActorSpec.Id (arole, aid), act, TargetSpec.Entity trole ->
              Ok (arole, (Some aid, (act, (trole, None))))
            | ActorSpec.Entity arole, act, TargetSpec.Id (trole, tid) ->
              Ok (arole, (None, (act, (trole, Some tid))))
            | ActorSpec.Id (arole, aid), act, TargetSpec.Id (trole, tid) ->
              Ok (arole, (Some aid, (act, (trole, Some tid))))
          in
          let decode (arole, (aid, (act, (trole, tid)))) =
            let open Guard in
            match aid, tid with
            | Some aid, Some tid ->
              Ok (ActorSpec.Id (arole, aid), act, TargetSpec.Id (trole, tid))
            | None, Some tid ->
              Ok (ActorSpec.Entity arole, act, TargetSpec.Id (trole, tid))
            | Some aid, None ->
              Ok (ActorSpec.Id (arole, aid), act, TargetSpec.Entity trole)
            | None, None ->
              Ok (ActorSpec.Entity arole, act, TargetSpec.Entity trole)
          in
          Caqti_type.(
            custom
              ~encode
              ~decode
              (tup2
                 Role.t
                 (tup2
                    (option Uuid.Actor.t)
                    (tup2 Action.t (tup2 Kind.t (option Uuid.Target.t))))))
        ;;

        let select_rule_sql =
          {sql|
            SELECT
              rules.actor_role,
              guardianDecodeUuid(rules.actor_uuid),
              rules.act,
              rules.target_role,
              guardianDecodeUuid(rules.target_uuid)
            FROM guardian_rules AS rules
          |sql}
        ;;

        let find_all ?ctx target_spec =
          match target_spec with
          | Guard.TargetSpec.Id (role, uuid) ->
            let query =
              select_rule_sql
              |> Format.asprintf
                   {sql|%s WHERE target_role = ? AND target_uuid = guardianEncodeUuid(?)|sql}
              |> Caqti_type.(tup2 Kind.t Uuid.Target.t ->* t)
            in
            Database.collect ?ctx query (role, uuid)
          | Guard.TargetSpec.Entity role ->
            let query =
              select_rule_sql
              |> Format.asprintf
                   {sql|%s WHERE target_role = ? AND target_uuid IS NULL |sql}
              |> Kind.t ->* t
            in
            Database.collect ?ctx query role
        ;;

        let find_all_of_entity ?ctx target_spec =
          match target_spec with
          | Guard.TargetSpec.Id (role, _) | Guard.TargetSpec.Entity role ->
            let query =
              select_rule_sql
              |> Format.asprintf {sql|%s WHERE target_role = ? |sql}
              |> Kind.t ->* t
            in
            Database.collect ?ctx query role
        ;;

        let act_on_rule ?ctx query rule =
          let query = Caqti_type.(t ->. unit) query in
          Database.exec ?ctx query rule |> Lwt_result.ok
        ;;

        let save ?ctx rule =
          let query =
            {sql|
              INSERT INTO guardian_rules (actor_role, actor_uuid, act, target_role, target_uuid)
              VALUES (?, guardianEncodeUuid(?), ?, ?, guardianEncodeUuid(?))
            |sql}
          in
          act_on_rule ?ctx query rule
        ;;

        let delete ?ctx rule =
          (* TODO: only mark as deleted *)
          let query =
            {sql|
              DELETE FROM guardian_rules
              WHERE actor_role = ?
                AND actor_uuid = guardianEncodeUuid(?)
                AND act = ?
                AND target_role = ?
                AND target_uuid = guardianEncodeUuid(?)
            |sql}
          in
          act_on_rule ?ctx query rule
        ;;
      end

      module Actor = struct
        let create ?ctx ?owner roles id =
          let caqti =
            {sql|
              INSERT INTO guardian_actors (uuid, owner)
              VALUES (guardianEncodeUuid(?), guardianEncodeUuid(?))
              ON DUPLICATE KEY UPDATE
                updated_at = NOW()
            |sql}
            |> Caqti_type.(tup2 Uuid.Actor.t (option Owner.t) ->. unit)
          in
          let%lwt () =
            Guard.RoleSet.to_list roles
            |> Lwt_list.iter_s (fun role -> Roles.upsert ?ctx (id, role))
          in
          Database.exec ?ctx caqti (id, owner) |> Lwt_result.ok
        ;;

        let mem ?ctx id =
          let caqti =
            {sql|SELECT TRUE FROM guardian_actors WHERE uuid = guardianEncodeUuid(?)|sql}
            |> Uuid.Actor.t ->? Caqti_type.bool
          in
          Database.find_opt ?ctx caqti id
          >|= CCOption.value ~default:false
          |> Lwt_result.ok
        ;;

        let find ?ctx id =
          let caqti =
            {sql|
              SELECT
                guardianDecodeUuid(uuid),
                guardianDecodeUuid(owner)
              FROM guardian_actors
              WHERE uuid = guardianEncodeUuid(?)
            |sql}
            |> Uuid.Actor.t ->? Caqti_type.(tup2 Uuid.Actor.t (option Owner.t))
          in
          Database.find_opt ?ctx caqti id
          >|= CCOption.to_result "Actor not found"
        ;;

        let find ?ctx typ id =
          let open Lwt_result.Syntax in
          let%lwt roles = Roles.find_by_actor ?ctx id in
          let* id, owner = find ?ctx id in
          Guard.Actor.make ?owner roles typ id |> Lwt.return_ok
        ;;

        let find_roles = Roles.find_by_actor
        let find_by_role = Roles.find_actors_by_role
        let find_by_roles = Roles.find_actors_by_roles

        let grant_roles ?ctx id =
          Guard.RoleSet.to_list
          %> Lwt_list.iter_s (fun role -> Roles.upsert ?ctx (id, role))
          %> Lwt_result.ok
        ;;

        let revoke_roles ?ctx id =
          (* TODO: only mark as deleted *)
          Guard.RoleSet.to_list
          %> Lwt_list.iter_s (fun role -> Roles.delete ?ctx (id, role))
          %> Lwt_result.ok
        ;;

        let find_owner ?ctx id =
          let caqti =
            {sql|
              SELECT
                guardianDecodeUuid(owner)
              FROM guardian_actors
              WHERE uuid = guardianEncodeUuid(?)
            |sql}
            |> Uuid.Actor.t ->? Owner.t
          in
          Database.find_opt ?ctx caqti id |> Lwt_result.ok
        ;;

        let save_owner ?ctx ?owner id =
          let caqti =
            Caqti_type.(tup2 (option Owner.t) Uuid.Actor.t ->. unit)
              {sql|
                UPDATE guardian_actors
                SET owner = guardianEncodeUuid(?)
                WHERE uuid = guardianEncodeUuid(?)
              |sql}
          in
          Database.exec ?ctx caqti (owner, id) |> Lwt_result.ok
        ;;
      end

      module Target = struct
        let create ?ctx ?owner kind id =
          let caqti =
            {sql|
              INSERT INTO guardian_targets (uuid, kind, owner)
              VALUES (guardianEncodeUuid(?), ?, guardianEncodeUuid(?))
              ON DUPLICATE KEY UPDATE
                updated_at = NOW()
            |sql}
            |> Caqti_type.(tup3 Uuid.Target.t Kind.t (option Owner.t) ->. unit)
          in
          Database.exec ?ctx caqti (id, kind, owner) |> Lwt_result.ok
        ;;

        let mem ?ctx id =
          let caqti =
            {sql|SELECT kind FROM guardian_targets WHERE uuid = guardianEncodeUuid(?)|sql}
            |> Uuid.Target.t ->? Kind.t
          in
          Database.find_opt ?ctx caqti id >|= CCOption.is_some |> Lwt_result.ok
        ;;

        let find_owner_base ?ctx typ id =
          let open Lwt.Infix in
          let caqti =
            {sql|
              SELECT
                guardianDecodeUuid(owner)
              FROM guardian_targets
              WHERE uuid = guardianEncodeUuid(?) AND kind = ?
            |sql}
            |> Caqti_type.(tup2 Uuid.Target.t Kind.t ->? option Owner.t)
          in
          Database.find_opt ?ctx caqti (id, typ) >|= CCOption.flatten
        ;;

        let find ?ctx typ id =
          let%lwt owner = find_owner_base ?ctx typ id in
          Guard.Target.make ?owner typ id |> Lwt.return_ok
        ;;

        let find_kind ?ctx id =
          let open Lwt.Infix in
          let caqti =
            {sql|SELECT kind FROM guardian_targets WHERE uuid = guardianEncodeUuid(?)|sql}
            |> Uuid.Target.t ->? Kind.t
          in
          Database.find_opt ?ctx caqti id
          >|= CCOption.to_result
                (Format.asprintf
                   "Target ('%s') not found - no kind"
                   ([%show: Uuid.Target.t] id))
        ;;

        let find_owner ?ctx typ = find_owner_base ?ctx typ %> Lwt_result.ok

        let save_owner ?ctx ?owner id =
          let caqti =
            {sql|
              UPDATE guardian_targets
              SET owner = guardianEncodeUuid(?)
              WHERE uuid = guardianEncodeUuid(?)
            |sql}
            |> Caqti_type.(tup2 (option Owner.t) Uuid.Target.t ->. unit)
          in
          Database.exec ?ctx caqti (owner, id) |> Lwt_result.ok
        ;;
      end

      module Relation = struct
        let find_related_query =
          ( Kind.t
          , {sql|
              WITH RECURSIVE cte_relations AS (
                SELECT id, origin, target, `query`
                FROM guardian_relations
                WHERE origin = ?
                UNION ALL
                SELECT r.id, r.origin, r.target, r.query
                FROM guardian_relations r
                JOIN cte_relations n ON r.origin = n.target
              )
              SELECT id FROM cte_relations
            |sql}
          )
        ;;

        let find_query_request =
          {sql|
            SELECT query
            FROM guardian_relations
            WHERE origin = ? AND target = ?
          |sql}
          |> Caqti_type.(tup2 Kind.t Kind.t ->? option string)
        ;;

        let find_query ?ctx origin target : (query option, string) Lwt_result.t =
          let open Lwt.Infix in
          Database.find_opt ?ctx find_query_request (origin, target)
          >|= function
          | Some (Some query) -> Ok (Some (Guard.Relation.Query.create query))
          | Some None -> Ok None
          | None ->
            let msg =
              Format.asprintf
                "Undefined Relation: %s -> %s"
                ([%show: TargetRoles.t] origin)
                ([%show: TargetRoles.t] target)
            in
            Error msg
        ;;

        let upsert_request =
          {sql|
              INSERT INTO guardian_relations (origin, target, query)
              VALUES (?, ?, ?)
              ON DUPLICATE KEY UPDATE
                query = VALUES(query),
                updated_at = NOW()
            |sql}
          |> Caqti_type.(tup3 Kind.t Kind.t (option Query.t) ->. unit)
        ;;

        let upsert ?ctx ?query origin target =
          Database.exec ?ctx upsert_request (origin, target, query)
          |> Lwt_result.ok
        ;;

        let find_rec_request =
          let subquery_input, subquery = find_related_query in
          Format.asprintf
            {sql|
              SELECT origin, target, query
              FROM guardian_relations
              WHERE id IN (%s)
            |sql}
            subquery
          |> Caqti_type.(subquery_input ->* tup3 Kind.t Kind.t (option Query.t))
        ;;

        let find_rec ?ctx kind = Database.collect ?ctx find_rec_request kind

        let create_rec_sql ?custom_where select_sql relations =
          let relation_sql =
            relations
            |> CCList.mapi (fun iter (origin, target, query) ->
                 CCOption.map_or
                   ~default:{sql| IS NULL |sql}
                   (Query.to_string
                    %> CCString.replace
                         ~sub:"?"
                         ~by:(Format.asprintf "@id_%s" (lowercase_role origin))
                    %> Format.asprintf {sql| IN (%s) |sql})
                   query
                 |> Format.asprintf
                      {sql|
                        SELECT @id_%s := t{{iterator}}.uuid, t{{iterator}}.kind, @pre_owner := t{{iterator}}.owner
                        FROM guardian_targets as t{{iterator}}
                        WHERE
                          t{{iterator}}.uuid = @pre_owner
                          OR t{{iterator}}.kind = '%s'
                          AND t{{iterator}}.uuid %s
                      |sql}
                      (lowercase_role target)
                      (TargetRoles.show target)
                 |> CCString.replace
                      ~sub:"{{iterator}}"
                      ~by:(CCInt.to_string iter))
          in
          let union = "\nUNION\n" in
          let origin_id, combined_sql =
            if CCList.is_empty relation_sql
            then "", ""
            else
              ( relations
                |> CCList.head_opt
                |> CCOption.map_or ~default:"" (fun (origin, _, _) ->
                     Format.asprintf "@id_%s :=" (lowercase_role origin))
              , CCString.concat union relation_sql
                |> Format.asprintf "%s%s" union )
          in
          Format.asprintf
            {sql|
              WITH RECURSIVE cte_find_effects (uuid, kind, `owner`) AS (
                SELECT %s uuid, kind, `owner`
                FROM guardian_targets
                %s
                %s
                GROUP BY uuid
              )
              %s
            |sql}
            origin_id
            (CCOption.value
               ~default:
                 {sql|WHERE kind = ? AND uuid = guardianEncodeUuid(?)|sql}
               custom_where)
            combined_sql
            select_sql
        ;;

        let create_rec_request =
          let open Caqti_type in
          let select_sql =
            {sql|
              SELECT
                kind,
                guardianDecodeUuid(uuid)
              FROM cte_find_effects
            |sql}
          in
          create_rec_sql select_sql
          %> (tup2 Kind.t Uuid.Target.t ->* tup2 Kind.t Uuid.Target.t)
        ;;

        let find_effects_rec ?ctx ((action, target_spec) : effect)
          : effect list Lwt.t
          =
          let open Guard.TargetSpec in
          let open Lwt.Infix in
          match target_spec with
          | Entity kind ->
            find_rec ?ctx kind
            >|= CCList.map (fun (_, target, _) -> action, Entity target)
          | Id (kind, id) ->
            let%lwt relations = find_rec ?ctx kind in
            let entity_specs =
              CCList.filter_map
                (fun (_, target, query) ->
                  if CCOption.is_none query then Some (Entity target) else None)
                relations
            in
            let request = create_rec_request relations in
            Database.collect ?ctx request (kind, id)
            >|= CCList.map (fun (kind, id) -> Id (kind, id))
            >|= CCList.append entity_specs
            >|= CCList.map (fun spec -> action, spec)
        ;;
      end

      let find_rules_of_spec_request_sql =
        Relation.create_rec_sql
          (Format.asprintf
             {sql|
                %s
                JOIN cte_find_effects as eff ON rules.target_role = eff.kind AND (target_uuid = eff.uuid OR target_uuid IS NULL)
              |sql}
             Rule.select_rule_sql)
      ;;

      let find_rules_of_spec_request =
        let open Caqti_type in
        find_rules_of_spec_request_sql %> (tup2 Kind.t Uuid.Target.t ->* Rule.t)
      ;;

      let cte_relations_sql =
        {sql|
          WITH RECURSIVE cte_relations AS (
            SELECT origin, target, query
            FROM guardian_relations
            WHERE origin = ?
            UNION ALL
            SELECT r.origin, r.target, r.query
            FROM guardian_relations r
            JOIN cte_relations n ON r.origin = n.target
          )
        |sql}
      ;;

      let find_rules_of_spec ?ctx ?(any_id = false) =
        let open Guard.TargetSpec in
        function
        | Entity kind ->
          let filter_uuid =
            if any_id then "" else {sql| AND rules.target_uuid IS NULL |sql}
          in
          let request =
            Format.asprintf
              {sql|
                %s
                %s
                JOIN cte_relations AS rel ON rules.target_role = rel.origin OR rules.target_role = rel.target %s
              |sql}
              cte_relations_sql
              Rule.select_rule_sql
              filter_uuid
            |> Kind.t ->* Rule.t
          in
          Database.collect ?ctx request kind
        | Id (kind, id) ->
          let%lwt relations = Relation.find_rec ?ctx kind in
          Database.collect ?ctx (find_rules_of_spec_request relations) (kind, id)
      ;;

      (** [define_validate_function] defines a 'validate<TargetRole>Uuid'
          function for mariadb.

          INPUT:

          - actor_uuid (binary 16)
          - action (enum: 'create','read','update','delete','manage')
          - target_uuid (binary 16, usually within the query)

          OUTPUT (Boolean):

          It returns true, if the actor has action rights on the specified
          target uuid. *)
      let define_validate_function ?ctx kind =
        let function_name =
          Format.asprintf "guardianValidate%sUuid" (kind |> capitalize_role)
        in
        let%lwt post_sql =
          let%lwt relations = Relation.find_rec ?ctx kind in
          let custom_where = {sql|WHERE uuid = id|sql} in
          Relation.create_rec_sql
            ~custom_where
            {sql|
              SELECT TRUE FROM guardian_rules AS rules
              JOIN cte_find_effects as eff
                ON rules.target_role = eff.kind
                  AND (rules.act = `action` OR rules.act = 'manage')
                  AND (target_uuid = eff.uuid OR target_uuid IS NULL)
                  AND rules.actor_role IN (
                    SELECT roles.role
                    FROM guardian_actor_roles AS roles
                    WHERE roles.actor_uuid = actor_id
                  )
              GROUP BY uuid
            |sql}
            relations
          |> Lwt.return
        in
        ( function_name
        , Format.asprintf
            {sql|
              CREATE FUNCTION %s(actor_id BINARY(16), `action` enum('create','read','update','delete','manage'), id BINARY(16)) RETURNS BOOLEAN
              BEGIN
                RETURN (%s);
              END
            |sql}
            function_name
            post_sql )
        |> Lwt.return
      ;;

      let define_validate_function_all ?ctx () =
        TargetRoles.all |> Lwt_list.map_s (define_validate_function ?ctx)
      ;;

      let validation_query ?(select_sql = "SELECT uuid") kind =
        ( Caqti_type.(tup2 Uuid.Actor.t Action.t)
        , Format.asprintf
            {sql|
              %s
              FROM guardian_targets
              WHERE kind = '%s'
              GROUP BY uuid
              HAVING guardianValidate%sUuid(guardianEncodeUuid(?), ?, uuid)
            |sql}
            select_sql
            (TargetRoles.show kind)
            (kind |> capitalize_role) )
      ;;

      let exists_for_kind ?ctx kind action actor : Uuid.Target.t list Lwt.t =
        let input_type, request =
          validation_query ~select_sql:"SELECT guardianDecodeUuid(uuid)" kind
        in
        Database.collect
          ?ctx
          (request |> input_type ->* Uuid.Target.t)
          (Guard.Actor.id actor, action)
      ;;

      let define_functions ?ctx () =
        let open Caqti_type in
        let%lwt define_all_validate_functions =
          define_validate_function_all ?ctx ()
        in
        define_encode_uuid
        :: define_decode_uuid
        :: define_all_validate_functions
        |> CCList.flat_map (fun (fcn_name, fcn) ->
             Format.asprintf {sql|DROP FUNCTION IF EXISTS %s|sql} fcn_name
             :: [ fcn ])
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
  end)
end
