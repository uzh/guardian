open Lwt.Infix
open Caqti_request.Infix

module Make (R : Guardian.Role_s) (Db : Database_pools.Sig) = struct
  module Guardian = Guardian.Make (R)

  include Guardian.Make_persistence (struct
    type role = R.t
    type role_set = Guardian.Role_set.t
    type 'a authorizable = 'a Guardian.Authorizable.t
    type auth_rule = Guardian.Authorizer.auth_rule
    type actor_spec = Guardian.Authorizer.actor_spec
    type ('rv, 'err) monad = ('rv, 'err) Lwt_result.t

    let find_roles ?ctx id : (Guardian.Role_set.t, string) Lwt_result.t =
      let caqti =
        {sql|SELECT roles FROM guardian_entities WHERE id = ?|sql}
        |> Caqti_type.(string ->! string)
      in
      let%lwt roles = Db.find ?ctx caqti (Uuidm.to_string id) in
      roles
      |> Yojson.Safe.from_string
      |> Guardian.Role_set.of_yojson
      |> Lwt.return
    ;;

    let find_rules ?ctx target_spec
      : (Guardian.Authorizer.auth_rule list, string) Lwt_result.t
      =
      let%lwt res =
        match target_spec with
        | `One uuid ->
          let caqti =
            {sql|
              SELECT act, actor_id, actor_role FROM guardian_rules
              WHERE target_id = ?
            |sql}
            |> Caqti_type.(
                 string ->* tup3 string (option string) (option string))
          in
          Db.collect ?ctx caqti (Uuidm.to_string uuid)
        | `Entity role ->
          let caqti =
            {sql|
              SELECT act, actor_id, actor_role FROM guardian_rules
              WHERE target_role = ?
            |sql}
            |> Caqti_type.(
                 string ->* tup3 string (option string) (option string))
          in
          Db.collect ?ctx caqti (R.show role)
      in
      CCList.map
        (fun (act, actor_id, actor_role) : Guardian.Authorizer.auth_rule ->
          let act = Guardian.Action.of_string act in
          match actor_id, CCOption.map R.of_string actor_role with
          | Some id, None ->
            (match Uuidm.of_string id with
             | Some id' -> `One id', act, target_spec
             | None -> raise (Failure (Format.asprintf "Invalid UUID: %s" id)))
          | None, Some role -> `Entity role, act, target_spec
          | Some _, Some _ | None, None ->
            raise
              (Failure
                 "Either both actor fields were occupied, or both were not."))
        res
      |> Lwt.return_ok
    ;;

    let act_on_rule ?ctx query (actor, act, target) =
      let spec_to_str = function
        | `One s -> Uuidm.to_string s
        | `Entity s -> R.show s
      in
      let actor' = spec_to_str actor in
      let act' = Guardian.Action.to_string act in
      let target' = spec_to_str target in
      let caqti = Caqti_type.(tup3 string string string ->. unit) @@ query in
      Db.exec ?ctx caqti (actor', act', target') |> Lwt_result.ok
    ;;

    let save_rule ?ctx (auth_rule : auth_rule) : (unit, string) Lwt_result.t =
      let query =
        match auth_rule with
        | `One _, _, `One _ ->
          {sql|INSERT INTO guardian_rules (actor_id, act, target_id) VALUES (?, ?, ?)|sql}
        | `One _, _, `Entity _ ->
          {sql|INSERT INTO guardian_rules (actor_id, act, target_role) VALUES (?, ?, ?)|sql}
        | `Entity _, _, `One _ ->
          {sql|INSERT INTO guardian_rules (actor_role, act, target_id) VALUES (?, ?, ?)|sql}
        | `Entity _, _, `Entity _ ->
          {sql|INSERT INTO guardian_rules (actor_role, act, target_role) VALUES (?, ?, ?)|sql}
      in
      act_on_rule ?ctx query auth_rule
    ;;

    let delete_rule ?ctx auth_rule =
      let query =
        match auth_rule with
        | `One _, _, `One _ ->
          {sql|DELETE FROM guardian_rules WHERE actor_id = ? AND act = ? AND target_id = ?|sql}
        | `One _, _, `Entity _ ->
          {sql|DELETE FROM guardian_rules WHERE actor_id = ? AND act = ? AND target_role = ?|sql}
        | `Entity _, _, `One _ ->
          {sql|DELETE FROM guardian_rules WHERE actor_role = ? AND act = ? AND target_id = ?|sql}
        | `Entity _, _, `Entity _ ->
          {sql|DELETE FROM guardian_rules WHERE actor_role = ? AND act = ? AND target_role = ?|sql}
      in
      act_on_rule ?ctx query auth_rule
    ;;

    let grant_roles ?ctx uuid roles =
      let open Lwt_result.Syntax in
      let open Guardian in
      let* pre_roles = find_roles ?ctx uuid in
      let roles' = Role_set.union roles pre_roles in
      if Role_set.(cardinal roles' > cardinal pre_roles)
      then (
        let caqti =
          Caqti_type.(tup2 string string ->. unit)
          @@ {sql|UPDATE guardian_entities SET roles = ? WHERE id = ?|sql}
        in
        let roles'' = Yojson.Safe.to_string (Role_set.to_yojson roles') in
        Db.exec ?ctx caqti (roles'', Uuidm.to_string uuid) |> Lwt_result.ok)
      else Lwt.return_ok ()
    ;;

    let revoke_roles ?ctx uuid roles =
      let open Lwt_result.Syntax in
      let open Guardian in
      let* pre_roles = find_roles ?ctx uuid in
      let roles' = Role_set.diff pre_roles roles in
      let caqti =
        Caqti_type.(tup2 string string ->. unit)
        @@ {sql|UPDATE guardian_entities SET roles = ? WHERE id = ?|sql}
      in
      let roles'' = Yojson.Safe.to_string (Role_set.to_yojson roles') in
      Db.exec ?ctx caqti (roles'', Uuidm.to_string uuid) |> Lwt_result.ok
    ;;

    let create_authorizable ?ctx ~id ?owner roles =
      let caqti =
        Caqti_type.(tup3 string string (option string) ->. unit)
        @@ {sql|INSERT INTO guardian_entities (id, roles, parent) VALUES (?, ?, ?)|sql}
      in
      let roles' = Guardian.Role_set.to_yojson roles |> Yojson.Safe.to_string in
      let owner' = CCOption.map Uuidm.to_string owner in
      Db.exec ?ctx caqti (Uuidm.to_string id, roles', owner') |> Lwt_result.ok
    ;;

    let mem_authorizable ?ctx id =
      let caqti =
        Caqti_type.(string ->? string)
        @@ {sql|SELECT roles FROM guardian_entities WHERE id = ?|sql}
      in
      Db.find_opt ?ctx caqti (Uuidm.to_string id)
      >|= CCOption.is_some
      |> Lwt_result.ok
    ;;

    let find_owner ?ctx id =
      let caqti =
        Caqti_type.(string ->! option string)
        @@ {sql|SELECT parent FROM guardian_entities WHERE id = ?|sql}
      in
      Db.find ?ctx caqti (Uuidm.to_string id)
      >|= CCFun.flip CCOption.bind Uuidm.of_string
      |> Lwt_result.ok
    ;;

    let save_owner ?ctx id ~owner =
      let caqti =
        Caqti_type.(tup2 string string ->. unit)
        @@ {sql|UPDATE guardian_entities SET parent = ? WHERE id = ?|sql}
      in
      Db.exec ?ctx caqti Uuidm.(to_string owner, to_string id) |> Lwt_result.ok
    ;;

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
