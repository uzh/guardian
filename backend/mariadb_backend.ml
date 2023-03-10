open CCFun.Infix
open Lwt.Infix
open Caqti_request.Infix

module Make
  (Actor : Guardian.RoleSig)
  (Target : Guardian.RoleSig)
  (Database : Database_pools.Sig) =
struct
  module Guard = Guardian.Make (Actor) (Target)
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

  module Role = BaseType (Actor)
  module Kind = BaseType (Target)

  module Roles = struct
    include Guard.ActorRoleSet

    let t =
      Caqti_type.(
        custom
          ~encode:(to_yojson %> Yojson.Safe.to_string %> CCResult.return)
          ~decode:(Yojson.Safe.from_string %> of_yojson)
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

  include Guard.Make_persistence (struct
    type 'a authorizable = 'a Guard.Authorizable.t
    type 'b authorizable_target = 'b Guard.AuthorizableTarget.t
    type role = Actor.t
    type actor_role_set = Roles.t
    type actor_spec = Guard.ActorSpec.t
    type auth_rule = Guard.Rule.t
    type auth_rule_set = Guard.AuthRuleSet.t
    type effect = Guard.Effect.t
    type auth_set = Guard.AuthenticationSet.t
    type target_spec = Guard.TargetSpec.t
    type target_typ = Target.t
    type ('rv, 'err) monad = ('rv, 'err) Lwt_result.t

    module Rule = struct
      include Guard.Rule

      let t =
        let encode = function
          | `ActorEntity arole, act, `TargetEntity trole ->
            Ok (arole, (None, (act, (trole, None))))
          | `Actor (arole, aid), act, `TargetEntity trole ->
            Ok (arole, (Some aid, (act, (trole, None))))
          | `ActorEntity arole, act, `Target (trole, tid) ->
            Ok (arole, (None, (act, (trole, Some tid))))
          | `Actor (arole, aid), act, `Target (trole, tid) ->
            Ok (arole, (Some aid, (act, (trole, Some tid))))
        in
        let decode (arole, (aid, (act, (trole, tid)))) =
          match aid, tid with
          | Some aid, Some tid ->
            Ok (`Actor (arole, aid), act, `Target (trole, tid))
          | None, Some tid -> Ok (`ActorEntity arole, act, `Target (trole, tid))
          | Some aid, None -> Ok (`Actor (arole, aid), act, `TargetEntity trole)
          | None, None -> Ok (`ActorEntity arole, act, `TargetEntity trole)
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
        | `Target (role, uuid) ->
          let where =
            {sql|WHERE target_role = ? AND target_id = UNHEX(REPLACE(?, '-', ''))|sql}
          in
          let caqti =
            select where |> Caqti_type.(tup2 Kind.t Uuid.Target.t ->* t)
          in
          Database.collect ?ctx caqti (role, uuid)
        | `TargetEntity role ->
          let where = {sql|WHERE target_role = ?|sql} in
          let caqti = select where |> Kind.t ->* t in
          Database.collect ?ctx caqti role
      ;;

      let act_on_rule ?ctx query rule =
        let caqti = Caqti_type.(t ->. unit) query in
        Database.exec ?ctx caqti rule |> Lwt_result.ok
      ;;

      let save ?ctx auth_rule =
        let query =
          {sql|
            INSERT INTO guardian_rules (actor_role, actor_id, act, target_role, target_id)
            VALUES (?, UNHEX(REPLACE(?, '-', '')), ?, ?, UNHEX(REPLACE(?, '-', '')))
          |sql}
        in
        act_on_rule ?ctx query auth_rule
      ;;

      let delete ?ctx auth_rule =
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
        act_on_rule ?ctx query auth_rule
      ;;
    end

    module Actor = struct
      module Authorizable = struct
        let create ?ctx ?owner roles id =
          let caqti =
            {sql|
              INSERT INTO guardian_actors (id, roles, parent)
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
      end

      let find ?ctx typ id =
        let open Lwt.Infix in
        let open Lwt_result.Syntax in
        let caqti =
          {sql|
            SELECT
              roles,
              LOWER(CONCAT(
                SUBSTR(HEX(parent), 1, 8), '-',
                SUBSTR(HEX(parent), 9, 4), '-',
                SUBSTR(HEX(parent), 13, 4), '-',
                SUBSTR(HEX(parent), 17, 4), '-',
                SUBSTR(HEX(parent), 21)
              ))
            FROM guardian_actors WHERE id = UNHEX(REPLACE(?, '-', ''))
          |sql}
          |> Caqti_type.(Uuid.Actor.t ->? tup2 Roles.t Owner.t)
        in
        let* roles, owner =
          Database.find_opt ?ctx caqti id
          >|= CCOption.to_result "No actor found."
        in
        Guard.Authorizable.make ?owner roles typ id |> Lwt.return_ok
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
                SUBSTR(HEX(parent), 1, 8), '-',
                SUBSTR(HEX(parent), 9, 4), '-',
                SUBSTR(HEX(parent), 13, 4), '-',
                SUBSTR(HEX(parent), 17, 4), '-',
                SUBSTR(HEX(parent), 21)
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
              SET parent = UNHEX(REPLACE(?, '-', ''))
              WHERE id = UNHEX(REPLACE(?, '-', ''))
            |sql}
        in
        Database.exec ?ctx caqti (owner, id) |> Lwt_result.ok
      ;;
    end

    module Target = struct
      module Authorizable = struct
        let create ?ctx ?owner kind id =
          let caqti =
            {sql|
              INSERT INTO guardian_targets (id, kind, parent)
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
      end

      let find ?ctx typ id =
        let open Lwt.Infix in
        let open Lwt_result.Syntax in
        let caqti =
          {sql|
            SELECT LOWER(CONCAT(
              SUBSTR(HEX(parent), 1, 8), '-',
              SUBSTR(HEX(parent), 9, 4), '-',
              SUBSTR(HEX(parent), 13, 4), '-',
              SUBSTR(HEX(parent), 17, 4), '-',
              SUBSTR(HEX(parent), 21)
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
        Guard.AuthorizableTarget.make ?owner typ id |> Lwt.return_ok
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
                SUBSTR(HEX(parent), 1, 8), '-',
                SUBSTR(HEX(parent), 9, 4), '-',
                SUBSTR(HEX(parent), 13, 4), '-',
                SUBSTR(HEX(parent), 17, 4), '-',
                SUBSTR(HEX(parent), 21)
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
            SET parent = UNHEX(REPLACE(?, '-', ''))
            WHERE id = UNHEX(REPLACE(?, '-', ''))
          |sql}
          |> Caqti_type.(tup2 Owner.t Uuid.Target.t ->. unit)
        in
        Database.exec ?ctx caqti (owner, id) |> Lwt_result.ok
      ;;
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
