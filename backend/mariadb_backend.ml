open Lwt.Infix
open Caqti_request.Infix

module Make
  (A : Guardian.RoleSig)
  (T : Guardian.RoleSig)
  (Db : Database_pools.Sig) =
struct
  open CCFun.Infix
  module Guardian = Guardian.Make (A) (T)
  module Authorizer = Guardian.Authorizer

  module Uuid = struct
    include Guardian.Uuid

    module Actor = struct
      include Actor

      let t =
        Caqti_type.(
          custom
            ~encode:(to_string %> CCResult.pure)
            ~decode:(fun id ->
              id
              |> of_string
              |> CCOption.to_result (Format.asprintf "Invalid UUID: %s" id))
            string)
      ;;
    end

    module Target = struct
      include Target

      let t =
        Caqti_type.(
          custom
            ~encode:(to_string %> CCResult.pure)
            ~decode:(fun id ->
              id
              |> of_string
              |> CCOption.to_result (Format.asprintf "Invalid UUID: %s" id))
            string)
      ;;
    end
  end

  module ActorSet = struct
    include Guardian.ActorRoleSet

    let t =
      Caqti_type.(
        custom
          ~encode:(to_yojson %> Yojson.Safe.to_string %> CCResult.pure)
          ~decode:(Yojson.Safe.from_string %> of_yojson)
          string)
    ;;
  end

  module TargetSet = struct
    include Guardian.TargetRoleSet

    let t =
      Caqti_type.(
        custom
          ~encode:(to_yojson %> Yojson.Safe.to_string %> CCResult.pure)
          ~decode:(Yojson.Safe.from_string %> of_yojson)
          string)
    ;;
  end

  module Owner = struct
    let t =
      let open CCResult in
      Caqti_type.(
        custom
          ~encode:(CCOption.map Uuid.Actor.to_string %> pure)
          ~decode:(CCFun.flip CCOption.bind Uuid.Actor.of_string %> pure)
          (option string))
    ;;
  end

  module ActorRole = struct
    type t = A.t

    let t =
      Caqti_type.(
        custom
          ~encode:(A.show %> CCResult.pure)
          ~decode:(A.of_string %> CCResult.pure)
          string)
    ;;
  end

  module TargetRole = struct
    type t = T.t

    let t =
      Caqti_type.(
        custom
          ~encode:(T.show %> CCResult.pure)
          ~decode:(T.of_string %> CCResult.pure)
          string)
    ;;
  end

  module Action = struct
    include Guardian.Action

    let t =
      Caqti_type.(
        custom
          ~encode:(to_string %> CCResult.pure)
          ~decode:(of_string %> CCResult.pure)
          string)
    ;;
  end

  module Rule = struct
    type t = Authorizer.auth_rule

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
             ActorRole.t
             (tup2
                (option Uuid.Actor.t)
                (tup2 Action.t (tup2 TargetRole.t (option Uuid.Target.t))))))
    ;;
  end

  include Guardian.Make_persistence (struct
    type 'a authorizable = 'a Guardian.Authorizable.t
    type 'b authorizable_target = 'b Guardian.AuthorizableTarget.t
    type role = A.t
    type auth_rule = Authorizer.auth_rule
    type actor_role_set = ActorSet.t
    type actor_spec = Authorizer.Actor.spec
    type target_role_set = TargetSet.t
    type target_spec = Authorizer.Target.spec
    type ('rv, 'err) monad = ('rv, 'err) Lwt_result.t

    module Actor = struct
      module Authorizable = struct
        let create ?ctx ?owner roles id =
          let caqti =
            Caqti_type.(tup3 Uuid.Actor.t ActorSet.t Owner.t ->. unit)
              {sql|INSERT INTO guardian_actors (id, roles, parent) VALUES (?, ?, ?)|sql}
          in
          Db.exec ?ctx caqti (id, roles, owner) |> Lwt_result.ok
        ;;

        let mem ?ctx id =
          let caqti =
            Caqti_type.(Uuid.Actor.t ->? string)
              {sql|SELECT roles FROM guardian_actors WHERE id = ?|sql}
          in
          Db.find_opt ?ctx caqti id >|= CCOption.is_some |> Lwt_result.ok
        ;;
      end

      let find ?ctx typ id =
        let open Lwt.Infix in
        let open Lwt_result.Syntax in
        let caqti =
          {sql|SELECT roles, parent FROM guardian_actors WHERE id = ?|sql}
          |> Caqti_type.(Uuid.Actor.t ->? tup2 ActorSet.t Owner.t)
        in
        let* roles, owner =
          Db.find_opt ?ctx caqti id >|= CCOption.to_result "No actor found."
        in
        Guardian.Authorizable.make ?owner roles typ id |> Lwt.return_ok
      ;;

      let find_roles ?ctx id =
        let open Lwt.Infix in
        let caqti =
          {sql|SELECT roles FROM guardian_actors WHERE id = ?|sql}
          |> Uuid.Actor.t ->? ActorSet.t
        in
        Db.find_opt ?ctx caqti id >|= CCOption.to_result "No actor roles found."
      ;;

      let find_rules ?ctx target_spec =
        let%lwt res =
          let select =
            {sql|SELECT actor_role, actor_id, act, target_role, target_id FROM guardian_rules|sql}
          in
          match target_spec with
          | `Target (role, uuid) ->
            let where = {sql|WHERE target_role = ? AND target_id = ?|sql} in
            let caqti =
              Format.asprintf "%s\n%s" select where
              |> Caqti_type.(tup2 TargetRole.t Uuid.Target.t ->* Rule.t)
            in
            Db.collect ?ctx caqti (role, uuid)
          | `TargetEntity role ->
            let where = {sql|WHERE target_role = ?|sql} in
            let caqti =
              Format.asprintf "%s\n%s" select where |> TargetRole.t ->* Rule.t
            in
            Db.collect ?ctx caqti role
        in
        res |> Lwt.return_ok
      ;;

      let act_on_rule ?ctx query rule =
        let caqti = Caqti_type.(Rule.t ->. unit) query in
        Db.exec ?ctx caqti rule |> Lwt_result.ok
      ;;

      let save_rule ?ctx auth_rule =
        let query =
          {sql|
            INSERT INTO guardian_rules (actor_role, actor_id, act, target_role, target_id)
            VALUES (?, ?, ?, ?, ?)
          |sql}
        in
        act_on_rule ?ctx query auth_rule
      ;;

      let delete_rule ?ctx auth_rule =
        let query =
          {sql|
            DELETE FROM guardian_rules
            WHERE actor_role = ? AND actor_id = ? AND act = ? AND target_role = ? AND target_id = ?
          |sql}
        in
        act_on_rule ?ctx query auth_rule
      ;;

      let grant_roles ?ctx uuid roles =
        let open Lwt_result.Syntax in
        let* pre_roles = find_roles ?ctx uuid in
        let roles' = ActorSet.union roles pre_roles in
        if ActorSet.(cardinal roles' > cardinal pre_roles)
        then (
          let caqti =
            Caqti_type.(tup2 ActorSet.t Uuid.Actor.t ->. unit)
              {sql|UPDATE guardian_actors SET roles = ? WHERE id = ?|sql}
          in
          Db.exec ?ctx caqti (roles', uuid) |> Lwt_result.ok)
        else Lwt.return_ok ()
      ;;

      let revoke_roles ?ctx uuid roles =
        let open Lwt_result.Syntax in
        let* pre_roles = find_roles ?ctx uuid in
        let roles' = ActorSet.diff pre_roles roles in
        let caqti =
          {sql|UPDATE guardian_actors SET roles = ? WHERE id = ?|sql}
          |> Caqti_type.(tup2 ActorSet.t Uuid.Actor.t ->. unit)
        in
        Db.exec ?ctx caqti (roles', uuid) |> Lwt_result.ok
      ;;

      let find_owner ?ctx id =
        let open Lwt.Infix in
        let caqti =
          {sql|SELECT parent FROM guardian_actors WHERE id = ?|sql}
          |> Uuid.Actor.t ->? Owner.t
        in
        Db.find_opt ?ctx caqti id >|= CCOption.flatten |> Lwt_result.ok
      ;;

      let save_owner ?ctx ?owner id =
        let caqti =
          Caqti_type.(tup2 Owner.t Uuid.Actor.t ->. unit)
            {sql|UPDATE guardian_actors SET parent = ? WHERE id = ?|sql}
        in
        Db.exec ?ctx caqti (owner, id) |> Lwt_result.ok
      ;;
    end

    module Target = struct
      module Authorizable = struct
        let create ?ctx ?owner roles id =
          let caqti =
            {sql|INSERT INTO guardian_targets (id, roles, parent) VALUES (?, ?, ?)|sql}
            |> Caqti_type.(tup3 Uuid.Target.t TargetSet.t Owner.t ->. unit)
          in
          Db.exec ?ctx caqti (id, roles, owner) |> Lwt_result.ok
        ;;

        let mem ?ctx id =
          let caqti =
            {sql|SELECT roles FROM guardian_targets WHERE id = ?|sql}
            |> Uuid.Target.t ->? TargetSet.t
          in
          Db.find_opt ?ctx caqti id >|= CCOption.is_some |> Lwt_result.ok
        ;;
      end

      let find ?ctx typ id =
        let open Lwt.Infix in
        let open Lwt_result.Syntax in
        let caqti =
          {sql|SELECT roles, parent FROM guardian_targets WHERE id = ?|sql}
          |> Caqti_type.(Uuid.Target.t ->? tup2 TargetSet.t Owner.t)
        in
        let* entity, owner =
          Db.find_opt ?ctx caqti id >|= CCOption.to_result "No target found."
        in
        Guardian.AuthorizableTarget.make ?owner entity typ id |> Lwt.return_ok
      ;;

      let find_roles ?ctx id =
        let open Lwt.Infix in
        let caqti =
          {sql|SELECT roles FROM guardian_targets WHERE id = ?|sql}
          |> Uuid.Target.t ->? TargetSet.t
        in
        Db.find_opt ?ctx caqti id
        >|= CCOption.to_result "No target roles found."
      ;;

      let find_owner ?ctx id =
        let open Lwt.Infix in
        let caqti =
          {sql|SELECT parent FROM guardian_targets WHERE id = ?|sql}
          |> Uuid.Target.t ->? Owner.t
        in
        Db.find_opt ?ctx caqti id
        >|= CCOption.to_result "No target found to return its owner."
      ;;

      let save_owner ?ctx ?owner id =
        let caqti =
          {sql|UPDATE guardian_targets SET parent = ? WHERE id = ?|sql}
          |> Caqti_type.(tup2 Owner.t Uuid.Target.t ->. unit)
        in
        Db.exec ?ctx caqti (owner, id) |> Lwt_result.ok
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
           Db.exec ?ctx (sql |> Caqti_type.(unit ->. unit)) ())
    ;;

    (** [clean ()] runs clean on a specified context [?ctx] **)
    let clean ?ctx () =
      ()
      |> find_clean
      |> Lwt_list.iter_s (fun (key, sql) ->
           Logs.debug (fun m -> m "Clean: Run '%s'" key);
           Db.exec ?ctx (sql |> Caqti_type.(unit ->. unit)) ())
    ;;
  end)
end
