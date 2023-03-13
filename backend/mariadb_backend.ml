open CCFun.Infix
open Lwt.Infix
open Caqti_request.Infix

module Make
  (ActorRoles : Guardian.RoleSig)
  (Target : Guardian.RoleSig)
  (Database : Database_pools.Sig) =
struct
  module Guard = Guardian.Make (ActorRoles) (Target)
  module Authorizer = Guard.Authorizer

  module BaseType (Core : Guardian.RoleSig) = struct
    include Core

    let t =
      let open CCResult in
      Caqti_type.(
        custom
          ~encode:(Core.show %> return)
          ~decode:(of_string %> return)
          string)
    ;;
  end

  module Uuid = struct
    include Guardian.Uuid

    module UuidBase (Core : Guardian.Uuid.Sig) = struct
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

    module Actor = UuidBase (Actor)
    module Target = UuidBase (Target)
  end

  module Owner = struct
    let t = Caqti_type.(option Uuid.Actor.t)
  end

  module Role = BaseType (ActorRoles)
  module Kind = BaseType (Target)

  module Roles = struct
    include Guard.RoleSet

    let t =
      Caqti_type.(
        custom
          ~encode:(to_yojson %> Yojson.Safe.to_string %> CCResult.return)
          ~decode:(Yojson.Safe.from_string %> of_yojson)
          string)
    ;;
  end

  module Action = struct
    include Guardian.Action

    let t =
      let open CCResult in
      Caqti_type.(
        custom
          ~encode:(Guardian.Action.show %> return)
          ~decode:(of_string %> return)
          string)
    ;;
  end

  include Guard.MakePersistence (struct
    type 'a actor = 'a Guard.Actor.t
    type 'b target = 'b Guard.Target.t
    type actor_spec = Guard.ActorSpec.t
    type effect = Guard.Effect.t
    type effect_set = Guard.EffectSet.t
    type kind = Target.t
    type parent_kind = Target.t
    type role_set = Roles.t
    type roles = ActorRoles.t
    type rule = Guard.Rule.t
    type target_spec = Guard.TargetSpec.t
    type ('rv, 'err) monad = ('rv, 'err) Lwt_result.t

    module Repo = struct
      module Rule = struct
        include Guard.Rule

        let t =
          let open Guard in
          let encode = function
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

        let find_all ?ctx target_spec =
          let open Guard in
          let select =
            Format.asprintf
              {sql|
              SELECT
                actor_role,
                LOWER(CONCAT(
                  SUBSTR(HEX(actor_id), 1, 8), '-',
                  SUBSTR(HEX(actor_id), 9, 4), '-',
                  SUBSTR(HEX(actor_id), 13, 4), '-',
                  SUBSTR(HEX(actor_id), 17, 4), '-',
                  SUBSTR(HEX(actor_id), 21)
                )),
                act,
                target_role,
                LOWER(CONCAT(
                  SUBSTR(HEX(target_id), 1, 8), '-',
                  SUBSTR(HEX(target_id), 9, 4), '-',
                  SUBSTR(HEX(target_id), 13, 4), '-',
                  SUBSTR(HEX(target_id), 17, 4), '-',
                  SUBSTR(HEX(target_id), 21)
                ))
              FROM guardian_rules
              %s
            |sql}
          in
          match target_spec with
          | TargetSpec.Id (role, uuid) ->
            let where =
              {sql|WHERE target_role = ? AND target_id = UNHEX(REPLACE(?, '-', ''))|sql}
            in
            let caqti =
              select where |> Caqti_type.(tup2 Kind.t Uuid.Target.t ->* t)
            in
            Database.collect ?ctx caqti (role, uuid)
          | TargetSpec.Entity role ->
            let where = {sql|WHERE target_role = ?|sql} in
            let caqti = select where |> Kind.t ->* t in
            Database.collect ?ctx caqti role
        ;;

        let act_on_rule ?ctx query rule =
          let caqti = Caqti_type.(t ->. unit) query in
          Database.exec ?ctx caqti rule |> Lwt_result.ok
        ;;

        let save ?ctx rule =
          let query =
            {sql|
            INSERT INTO guardian_rules (actor_role, actor_id, act, target_role, target_id)
            VALUES (?, UNHEX(REPLACE(?, '-', '')), ?, ?, UNHEX(REPLACE(?, '-', '')))
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
              AND actor_id = UNHEX(REPLACE(?, '-', ''))
              AND act = ?
              AND target_role = ?
              AND target_id = UNHEX(REPLACE(?, '-', ''))
          |sql}
          in
          act_on_rule ?ctx query rule
        ;;
      end

      module Actor = struct
        let create ?ctx ?owner roles id =
          let caqti =
            {sql|
              INSERT INTO guardian_actors (id, roles, owner)
              VALUES (UNHEX(REPLACE(?, '-', '')), ?, UNHEX(REPLACE(?, '-', '')))
              ON DUPLICATE KEY UPDATE
                updated_at = NOW()
            |sql}
            |> Caqti_type.(tup3 Uuid.Actor.t Roles.t Owner.t ->. unit)
          in
          Database.exec ?ctx caqti (id, roles, owner) |> Lwt_result.ok
        ;;

        let mem ?ctx id =
          let caqti =
            {sql|SELECT roles FROM guardian_actors WHERE id = UNHEX(REPLACE(?, '-', ''))|sql}
            |> Uuid.Actor.t ->? Caqti_type.string
          in
          Database.find_opt ?ctx caqti id >|= CCOption.is_some |> Lwt_result.ok
        ;;

        let find ?ctx typ id =
          let open Lwt.Infix in
          let open Lwt_result.Syntax in
          let caqti =
            {sql|
            SELECT
              roles,
              LOWER(CONCAT(
                SUBSTR(HEX(owner), 1, 8), '-',
                SUBSTR(HEX(owner), 9, 4), '-',
                SUBSTR(HEX(owner), 13, 4), '-',
                SUBSTR(HEX(owner), 17, 4), '-',
                SUBSTR(HEX(owner), 21)
              ))
            FROM guardian_actors WHERE id = UNHEX(REPLACE(?, '-', ''))
          |sql}
            |> Caqti_type.(Uuid.Actor.t ->? tup2 Roles.t Owner.t)
          in
          let* roles, owner =
            Database.find_opt ?ctx caqti id
            >|= CCOption.to_result "No actor found."
          in
          Guard.Actor.make ?owner roles typ id |> Lwt.return_ok
        ;;

        let find_roles ?ctx id =
          let open Lwt.Infix in
          let caqti =
            {sql|SELECT roles FROM guardian_actors WHERE id = UNHEX(REPLACE(?, '-', ''))|sql}
            |> Uuid.Actor.t ->? Roles.t
          in
          Database.find_opt ?ctx caqti id
          >|= CCOption.to_result "No actor roles found."
        ;;

        let update_roles_request ?ctx uuid roles =
          let caqti =
            {sql|UPDATE guardian_actors SET roles = ? WHERE id = UNHEX(REPLACE(?, '-', ''))|sql}
            |> Caqti_type.(tup2 Roles.t Uuid.Actor.t ->. unit)
          in
          Database.exec ?ctx caqti (roles, uuid) |> Lwt_result.ok
        ;;

        let grant_roles ?ctx uuid roles =
          let open Lwt_result.Syntax in
          let* pre_roles = find_roles ?ctx uuid in
          let roles' = Roles.union roles pre_roles in
          if Roles.(cardinal roles' > cardinal pre_roles)
          then update_roles_request ?ctx uuid roles'
          else Lwt.return_ok ()
        ;;

        let revoke_roles ?ctx uuid roles =
          let open Lwt_result.Syntax in
          (* TODO: only mark as deleted -> add second revoked_roles column *)
          let* pre_roles = find_roles ?ctx uuid in
          let roles' = Roles.diff pre_roles roles in
          update_roles_request ?ctx uuid roles'
        ;;

        let find_owner ?ctx id =
          let open Lwt.Infix in
          let caqti =
            {sql|
            SELECT
              LOWER(CONCAT(
                SUBSTR(HEX(owner), 1, 8), '-',
                SUBSTR(HEX(owner), 9, 4), '-',
                SUBSTR(HEX(owner), 13, 4), '-',
                SUBSTR(HEX(owner), 17, 4), '-',
                SUBSTR(HEX(owner), 21)
              ))
            FROM guardian_actors
            WHERE id = UNHEX(REPLACE(?, '-', ''))
          |sql}
            |> Uuid.Actor.t ->? Owner.t
          in
          Database.find_opt ?ctx caqti id >|= CCOption.flatten |> Lwt_result.ok
        ;;

        let save_owner ?ctx ?owner id =
          let caqti =
            Caqti_type.(tup2 Owner.t Uuid.Actor.t ->. unit)
              {sql|
              UPDATE guardian_actors
              SET owner = UNHEX(REPLACE(?, '-', ''))
              WHERE id = UNHEX(REPLACE(?, '-', ''))
            |sql}
          in
          Database.exec ?ctx caqti (owner, id) |> Lwt_result.ok
        ;;
      end

      module Target = struct
        let create ?ctx ?owner kind id =
          let caqti =
            {sql|
              INSERT INTO guardian_targets (id, kind, owner)
              VALUES (UNHEX(REPLACE(?, '-', '')), ?, UNHEX(REPLACE(?, '-', '')))
              ON DUPLICATE KEY UPDATE
                updated_at = NOW()
            |sql}
            |> Caqti_type.(tup3 Uuid.Target.t Kind.t Owner.t ->. unit)
          in
          Database.exec ?ctx caqti (id, kind, owner) |> Lwt_result.ok
        ;;

        let mem ?ctx id =
          let caqti =
            {sql|SELECT kind FROM guardian_targets WHERE id = UNHEX(REPLACE(?, '-', ''))|sql}
            |> Uuid.Target.t ->? Kind.t
          in
          Database.find_opt ?ctx caqti id >|= CCOption.is_some |> Lwt_result.ok
        ;;

        let find ?ctx typ id =
          let open Lwt.Infix in
          let open Lwt_result.Syntax in
          let caqti =
            {sql|
            SELECT LOWER(CONCAT(
              SUBSTR(HEX(owner), 1, 8), '-',
              SUBSTR(HEX(owner), 9, 4), '-',
              SUBSTR(HEX(owner), 13, 4), '-',
              SUBSTR(HEX(owner), 17, 4), '-',
              SUBSTR(HEX(owner), 21)
            ))
            FROM guardian_targets
            WHERE id = UNHEX(REPLACE(?, '-', '')) AND kind = ?
          |sql}
            |> Caqti_type.(tup2 Uuid.Target.t Kind.t ->? Owner.t)
          in
          let* owner =
            Database.find_opt ?ctx caqti (id, typ)
            >|= CCOption.to_result "No target found."
          in
          Guard.Target.make ?owner typ id |> Lwt.return_ok
        ;;

        let find_kind ?ctx id =
          let open Lwt.Infix in
          let caqti =
            {sql|SELECT kind FROM guardian_targets WHERE id = UNHEX(REPLACE(?, '-', ''))|sql}
            |> Uuid.Target.t ->? Kind.t
          in
          Database.find_opt ?ctx caqti id
          >|= CCOption.to_result "No target kind found."
        ;;

        let find_owner ?ctx id =
          let open Lwt.Infix in
          let caqti =
            {sql|
            SELECT
              LOWER(CONCAT(
                SUBSTR(HEX(owner), 1, 8), '-',
                SUBSTR(HEX(owner), 9, 4), '-',
                SUBSTR(HEX(owner), 13, 4), '-',
                SUBSTR(HEX(owner), 17, 4), '-',
                SUBSTR(HEX(owner), 21)
              ))
            FROM guardian_targets
            WHERE id = UNHEX(REPLACE(?, '-', ''))
          |sql}
            |> Uuid.Target.t ->? Owner.t
          in
          Database.find_opt ?ctx caqti id
          >|= CCOption.to_result "No target found to return its owner."
        ;;

        let save_owner ?ctx ?owner id =
          let caqti =
            {sql|
            UPDATE guardian_targets
            SET owner = UNHEX(REPLACE(?, '-', ''))
            WHERE id = UNHEX(REPLACE(?, '-', ''))
          |sql}
            |> Caqti_type.(tup2 Owner.t Uuid.Target.t ->. unit)
          in
          Database.exec ?ctx caqti (owner, id) |> Lwt_result.ok
        ;;
      end
    end

    (** [find_migrations ()] returns a list of all migrations as a tuple with
        key, datetime and sql query **)
    let find_migrations () = Migrations.all

    (** [find_clean ()] returns a list of all migrations as a tuple with key and
        sql query **)
    let find_clean () =
      Migrations.all_tables
      |> CCList.map (fun m -> m, Format.asprintf "TRUNCATE TABLE %s" m)
    ;;

    (** [migrate ()] runs all migration on a specified context [?ctx] **)
    let migrate ?ctx () =
      ()
      |> find_migrations
      |> Lwt_list.iter_s (fun (key, date, sql) ->
           Logs.debug (fun m -> m "Migration: Run '%s' from '%s'" key date);
           Database.exec ?ctx (sql |> Caqti_type.(unit ->. unit)) ())
    ;;

    (** [clean ()] runs clean on a specified context [?ctx] **)
    let clean ?ctx () =
      ()
      |> find_clean
      |> Lwt_list.iter_s (fun (key, sql) ->
           Logs.debug (fun m -> m "Clean: Run '%s'" key);
           Database.exec ?ctx (sql |> Caqti_type.(unit ->. unit)) ())
    ;;
  end)
end
