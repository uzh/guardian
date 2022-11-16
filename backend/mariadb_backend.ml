open Lwt.Infix
open Caqti_request.Infix

module Make
  (A : Guardian.RoleSig)
  (T : Guardian.RoleSig)
  (Db : Database_pools.Sig) =
struct
  module Guardian = Guardian.Make (A) (T)
  module ActorSet = Guardian.ActorRoleSet
  module Authorizer = Guardian.Authorizer
  module TargetSet = Guardian.TargetRoleSet
  module Uuid = Guardian.Uuid

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
            Caqti_type.(tup3 string string (option string) ->. unit)
              {sql|INSERT INTO guardian_actors (id, roles, parent) VALUES (?, ?, ?)|sql}
          in
          let roles' = ActorSet.to_yojson roles |> Yojson.Safe.to_string in
          let owner' = CCOption.map Uuid.Actor.to_string owner in
          Db.exec ?ctx caqti (Uuid.Actor.to_string id, roles', owner')
          |> Lwt_result.ok
        ;;

        let mem ?ctx id =
          let caqti =
            Caqti_type.(string ->? string)
              {sql|SELECT roles FROM guardian_actors WHERE id = ?|sql}
          in
          Db.find_opt ?ctx caqti (Uuid.Actor.to_string id)
          >|= CCOption.is_some
          |> Lwt_result.ok
        ;;
      end

      let find ?ctx typ id =
        let open Lwt_result.Syntax in
        let caqti =
          {sql|SELECT roles, parent FROM guardian_actors WHERE id = ?|sql}
          |> Caqti_type.(string ->! tup2 string (option string))
        in
        let%lwt roles, owner = Db.find ?ctx caqti (Uuid.Actor.to_string id) in
        let* roles =
          roles
          |> Yojson.Safe.from_string
          |> ActorSet.of_yojson
          |> Lwt_result.lift
        in
        let owner = owner |> CCFun.flip CCOption.bind Uuid.Actor.of_string in
        Guardian.Authorizable.make ?owner roles typ id |> Lwt.return_ok
      ;;

      let find_roles ?ctx id =
        let caqti =
          {sql|SELECT roles FROM guardian_actors WHERE id = ?|sql}
          |> Caqti_type.(string ->! string)
        in
        let%lwt roles = Db.find ?ctx caqti (Uuid.Actor.to_string id) in
        roles |> Yojson.Safe.from_string |> ActorSet.of_yojson |> Lwt.return
      ;;

      let find_rules ?ctx target_spec =
        let%lwt res =
          match target_spec with
          | `Target uuid ->
            let caqti =
              {sql|
              SELECT act, actor_id, actor_role FROM guardian_rules
              WHERE target_id = ?
            |sql}
              |> Caqti_type.(
                   string ->* tup3 string (option string) (option string))
            in
            Db.collect ?ctx caqti (Uuid.Target.to_string uuid)
          | `TargetEntity role ->
            let caqti =
              {sql|
              SELECT act, actor_id, actor_role FROM guardian_rules
              WHERE target_role = ?
            |sql}
              |> Caqti_type.(
                   string ->* tup3 string (option string) (option string))
            in
            Db.collect ?ctx caqti (role |> T.show)
        in
        CCList.map
          (fun (act, actor_id, actor_role) ->
            let act = Guardian.Action.of_string act in
            match actor_id, CCOption.map A.of_string actor_role with
            | Some id, None ->
              (match Uuid.Actor.of_string id with
               | Some id' -> `Actor id', act, target_spec
               | None -> raise (Failure (Format.asprintf "Invalid UUID: %s" id)))
            | None, Some role -> `ActorEntity role, act, target_spec
            | Some _, Some _ | None, None ->
              raise
                (Failure
                   "Either both actor fields were occupied, or both were not."))
          res
        |> Lwt.return_ok
      ;;

      let act_on_rule ?ctx query (actor, action, target) =
        let open Guardian in
        let actor' = Authorizer.Actor.value actor in
        let action' = Action.to_string action in
        let target' = Authorizer.Target.value target in
        let caqti = Caqti_type.(tup3 string string string ->. unit) query in
        Db.exec ?ctx caqti (actor', action', target') |> Lwt_result.ok
      ;;

      let save_rule ?ctx auth_rule =
        let query =
          match auth_rule with
          | `Actor _, _, `Target _ ->
            {sql|INSERT INTO guardian_rules (actor_id, act, target_id) VALUES (?, ?, ?)|sql}
          | `Actor _, _, `TargetEntity _ ->
            {sql|INSERT INTO guardian_rules (actor_id, act, target_role) VALUES (?, ?, ?)|sql}
          | `ActorEntity _, _, `Target _ ->
            {sql|INSERT INTO guardian_rules (actor_role, act, target_id) VALUES (?, ?, ?)|sql}
          | `ActorEntity _, _, `TargetEntity _ ->
            {sql|INSERT INTO guardian_rules (actor_role, act, target_role) VALUES (?, ?, ?)|sql}
        in
        act_on_rule ?ctx query auth_rule
      ;;

      let delete_rule ?ctx auth_rule =
        let query =
          match auth_rule with
          | `Actor _, _, `Target _ ->
            {sql|DELETE FROM guardian_rules WHERE actor_id = ? AND act = ? AND target_id = ?|sql}
          | `Actor _, _, `TargetEntity _ ->
            {sql|DELETE FROM guardian_rules WHERE actor_id = ? AND act = ? AND target_role = ?|sql}
          | `ActorEntity _, _, `Target _ ->
            {sql|DELETE FROM guardian_rules WHERE actor_role = ? AND act = ? AND target_id = ?|sql}
          | `ActorEntity _, _, `TargetEntity _ ->
            {sql|DELETE FROM guardian_rules WHERE actor_role = ? AND act = ? AND target_role = ?|sql}
        in
        act_on_rule ?ctx query auth_rule
      ;;

      let grant_roles ?ctx uuid roles =
        let open Lwt_result.Syntax in
        let open Guardian in
        let* pre_roles = find_roles ?ctx uuid in
        let roles' = ActorSet.union roles pre_roles in
        if ActorSet.(cardinal roles' > cardinal pre_roles)
        then (
          let caqti =
            Caqti_type.(tup2 string string ->. unit)
              {sql|UPDATE guardian_actors SET roles = ? WHERE id = ?|sql}
          in
          let roles'' = Yojson.Safe.to_string (ActorSet.to_yojson roles') in
          Db.exec ?ctx caqti (roles'', Uuid.Actor.to_string uuid)
          |> Lwt_result.ok)
        else Lwt.return_ok ()
      ;;

      let revoke_roles ?ctx uuid roles =
        let open Lwt_result.Syntax in
        let open Guardian in
        let* pre_roles = find_roles ?ctx uuid in
        let roles' = ActorSet.diff pre_roles roles in
        let caqti =
          Caqti_type.(tup2 string string ->. unit)
            {sql|UPDATE guardian_actors SET roles = ? WHERE id = ?|sql}
        in
        let roles'' = Yojson.Safe.to_string (ActorSet.to_yojson roles') in
        Db.exec ?ctx caqti (roles'', Uuid.Actor.to_string uuid) |> Lwt_result.ok
      ;;

      let find_owner ?ctx id =
        let caqti =
          Caqti_type.(string ->! option string)
            {sql|SELECT parent FROM guardian_actors WHERE id = ?|sql}
        in
        Db.find ?ctx caqti (Uuid.Actor.to_string id)
        >|= CCFun.flip CCOption.bind Uuid.Actor.of_string
        |> Lwt_result.ok
      ;;

      let save_owner ?ctx ?owner id =
        let caqti =
          Caqti_type.(tup2 (option string) string ->. unit)
            {sql|UPDATE guardian_actors SET parent = ? WHERE id = ?|sql}
        in
        Db.exec
          ?ctx
          caqti
          Uuid.Actor.(owner |> CCOption.map to_string, to_string id)
        |> Lwt_result.ok
      ;;
    end

    module Target = struct
      module Authorizable = struct
        let create ?ctx ?owner roles id =
          let caqti =
            Caqti_type.(tup3 string string (option string) ->. unit)
              {sql|INSERT INTO guardian_targets (id, roles, parent) VALUES (?, ?, ?)|sql}
          in
          let roles' = TargetSet.to_yojson roles |> Yojson.Safe.to_string in
          let owner' = owner |> CCOption.map Uuid.Actor.to_string in
          Db.exec ?ctx caqti (Uuid.Target.to_string id, roles', owner')
          |> Lwt_result.ok
        ;;

        let mem ?ctx id =
          let caqti =
            Caqti_type.(string ->? string)
              {sql|SELECT roles FROM guardian_targets WHERE id = ?|sql}
          in
          Db.find_opt ?ctx caqti (Uuid.Target.to_string id)
          >|= CCOption.is_some
          |> Lwt_result.ok
        ;;
      end

      let find ?ctx typ id =
        let open Lwt_result.Syntax in
        let caqti =
          {sql|SELECT roles, parent FROM guardian_targets WHERE id = ?|sql}
          |> Caqti_type.(string ->! tup2 string string)
        in
        let%lwt entity, owner = Db.find ?ctx caqti (Uuid.Target.to_string id) in
        let* entity =
          entity
          |> Yojson.Safe.from_string
          |> TargetSet.of_yojson
          |> Lwt_result.lift
        in
        let owner = owner |> Uuid.Actor.of_string in
        Guardian.AuthorizableTarget.make id owner typ entity |> Lwt.return_ok
      ;;

      let find_roles ?ctx id =
        let caqti =
          {sql|SELECT roles FROM guardian_targets WHERE id = ?|sql}
          |> Caqti_type.(string ->! string)
        in
        let%lwt roles = Db.find ?ctx caqti (Uuid.Target.to_string id) in
        roles |> Yojson.Safe.from_string |> TargetSet.of_yojson |> Lwt.return
      ;;

      let find_owner ?ctx id =
        let caqti =
          Caqti_type.(string ->! option string)
            {sql|SELECT parent FROM guardian_targets WHERE id = ?|sql}
        in
        Db.find ?ctx caqti (Uuid.Target.to_string id)
        >|= CCFun.flip CCOption.bind Uuid.Actor.of_string
        |> Lwt_result.ok
      ;;

      let save_owner ?ctx ?owner id =
        let caqti =
          Caqti_type.(tup2 (option string) string ->. unit)
            {sql|UPDATE guardian_targets SET parent = ? WHERE id = ?|sql}
        in
        Db.exec
          ?ctx
          caqti
          Uuid.(owner |> CCOption.map Actor.to_string, Target.to_string id)
        |> Lwt_result.ok
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
