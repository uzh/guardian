module Make (R : Guardian.Role_s) = struct
  module Guardian = Guardian.Make (R)

  let ( let* ) = Lwt_result.bind

  (** TODO: generalize this. right now it's fine because this backend should
      really only be used for testing purposes, but in the future it would be
      nice to have actual sqlite3 support. *)
  let db = Sqlite3.db_open "test_db.sqlite3"

  module Backend = struct
    type role = R.t
    type role_set = Guardian.Role_set.t
    type 'a authorizable = 'a Guardian.Authorizable.t
    type auth_rule = Guardian.Authorizer.auth_rule
    type actor_spec = Guardian.Authorizer.actor_spec
    type ('rv, 'err) monad = ('rv, 'err) Lwt_result.t

    let lwt_return_rc = function
      | Sqlite3.Rc.OK | DONE | ROW -> Lwt.return_ok ()
      | rc -> Lwt.return_error (Sqlite3.Rc.to_string rc)

    let create_authorizable ~id ?(owner : Uuidm.t option) roles :
        (unit, string) Lwt_result.t =
      let stmt =
        Sqlite3.prepare db
          "INSERT INTO entities (id, roles, parent) VALUES (?, ?, ?)"
      in
      let roles' = Guardian.Role_set.to_yojson roles |> Yojson.Safe.to_string in
      let* () =
        let open Sqlite3 in
        bind_values stmt
          [
            Data.TEXT (Uuidm.to_string id);
            Data.TEXT roles';
            Data.opt_text (Option.map Uuidm.to_string owner);
          ]
        |> lwt_return_rc
      in
      lwt_return_rc (Sqlite3.step stmt)

    let get_roles id =
      let id' = Uuidm.to_string id in
      let stmt = Sqlite3.prepare db "SELECT roles FROM entities WHERE id = ?" in
      let _ =
        let open Sqlite3 in
        bind_values stmt [ Data.TEXT id' ]
      in
      let* () = lwt_return_rc (Sqlite3.step stmt) in
      match String.trim (Sqlite3.column_text stmt 0) with
      | "" -> Lwt.return_ok Guardian.Role_set.empty
      | coltext ->
          Yojson.Safe.from_string coltext
          |> Guardian.Role_set.of_yojson |> Lwt.return

    let get_owner id =
      let id' = Uuidm.to_string id in
      let stmt =
        Sqlite3.prepare db "SELECT parent FROM entities WHERE id = ?"
      in
      let* () =
        let open Sqlite3 in
        let _ = bind_values stmt [ Data.TEXT id' ] in
        lwt_return_rc (step stmt)
      in
      let raw = Sqlite3.column_text stmt 0 in
      match Uuidm.of_string raw with
      | Some uuid -> Lwt.return_ok (Some uuid)
      | None -> Lwt.return_ok None

    let put_perm ((actor, action) : auth_rule) =
      let action' = Guardian.Action.to_string action in
      let target = Guardian.Authorizer.to_target_actor_spec action in
      let stmt =
        let open Sqlite3 in
        match (actor, target) with
        | `One auuid, `One tuuid ->
            let stmt =
              {|INSERT INTO rules (actor_id, act, target_id)
              VALUES (?, ?, ?)|}
              |> prepare db
            in
            let _ =
              bind_values stmt
                Data.
                  [
                    TEXT (Uuidm.to_string auuid);
                    TEXT action';
                    TEXT (Uuidm.to_string tuuid);
                  ]
            in
            stmt
        | `One auuid, `Entity trole ->
            let stmt =
              {|INSERT INTO rules (actor_id, act, target_role)
              VALUES (?, ?, ?)|}
              |> prepare db
            in
            let _ =
              bind_values stmt
                Data.
                  [
                    TEXT (Uuidm.to_string auuid);
                    TEXT action';
                    TEXT (R.show trole);
                  ]
            in
            stmt
        | `Entity arole, `One tuuid ->
            let stmt =
              {|INSERT INTO rules (actor_role, act, target_id)
              VALUES (?, ?, ?)|}
              |> prepare db
            in
            let _ =
              bind_values stmt
                Data.
                  [
                    TEXT (R.show arole);
                    TEXT action';
                    TEXT (Uuidm.to_string tuuid);
                  ]
            in
            stmt
        | `Entity arole, `Entity trole ->
            let stmt =
              {|INSERT INTO rules (actor_role, act, target_role)
              VALUES (?, ?, ?)|}
              |> prepare db
            in
            let _ =
              bind_values stmt
                Data.[ TEXT (R.show arole); TEXT action'; TEXT (R.show trole) ]
            in
            stmt
      in
      lwt_return_rc (Sqlite3.step stmt)

    let delete_perm ((actor, action) : Guardian.Authorizer.auth_rule) =
      let target = Guardian.Authorizer.to_target_actor_spec action in

      match (actor, target) with
      | `One aid, `One tid ->
          let stmt =
            "DELETE FROM rules WHERE actor_id=? AND act=? AND target_id=?"
          in
          let stmt = Sqlite3.prepare db stmt in
          let* () =
            lwt_return_rc
              Sqlite3.(
                bind_values stmt
                  Data.
                    [
                      TEXT (Uuidm.to_string aid);
                      TEXT (Guardian.Action.to_string action);
                      TEXT (Uuidm.to_string tid);
                    ])
          in
          lwt_return_rc (Sqlite3.step stmt)
      | `One aid, `Entity trole ->
          let stmt =
            "DELETE FROM rules WHERE actor_id=? AND act=? AND target_role=?"
          in
          let stmt = Sqlite3.prepare db stmt in
          let* () =
            lwt_return_rc
              Sqlite3.(
                bind_values stmt
                  Data.
                    [
                      TEXT (Uuidm.to_string aid);
                      TEXT (Guardian.Action.to_string action);
                      TEXT (R.show trole);
                    ])
          in
          lwt_return_rc (Sqlite3.step stmt)
      | `Entity arole, `One tid ->
          let stmt =
            "DELETE FROM rules WHERE actor_role=? AND act=? AND target_id=?"
          in
          let stmt = Sqlite3.prepare db stmt in
          let* () =
            lwt_return_rc
              Sqlite3.(
                bind_values stmt
                  Data.
                    [
                      TEXT (R.show arole);
                      TEXT (Guardian.Action.to_string action);
                      TEXT (Uuidm.to_string tid);
                    ])
          in
          lwt_return_rc (Sqlite3.step stmt)
      | `Entity arole, `Entity trole ->
          let stmt =
            "DELETE FROM rules WHERE actor_role=? AND act=? AND target_role=?"
          in
          let stmt = Sqlite3.prepare db stmt in
          let* () =
            lwt_return_rc
              Sqlite3.(
                bind_values stmt
                  Data.
                    [
                      TEXT (R.show arole);
                      TEXT (Guardian.Action.to_string action);
                      TEXT (R.show trole);
                    ])
          in
          lwt_return_rc (Sqlite3.step stmt)

    let get_perms (spec : actor_spec) =
      let* stmt =
        let open Sqlite3 in
        match spec with
        | `One uuidm ->
            let stmt =
              "SELECT act, actor_id, actor_role FROM rules WHERE target_id = ?"
              |> prepare db
            in
            let* () =
              lwt_return_rc
                (bind_values stmt [ Data.TEXT (Uuidm.to_string uuidm) ])
            in
            Lwt.return_ok stmt
        | `Entity entity ->
            let stmt =
              "SELECT act, actor_id, actor_role FROM rules WHERE target_role = \
               ?" |> prepare db
            in
            let () = Printf.printf "Searching for role: %s\n" (R.show entity) in
            let* () =
              lwt_return_rc (bind_values stmt [ Data.TEXT (R.show entity) ])
            in
            Lwt.return_ok stmt
      in
      let rc, rv =
        Sqlite3.fold stmt
          ~f:(fun acc row ->
            let action =
              row.(0) |> Sqlite3.Data.to_string_exn
              |> Guardian.Action.of_string spec
            in
            let (actor_spec : Guardian.Authorizer.actor_spec) =
              match Sqlite3.Data.(to_string row.(1), to_string row.(2)) with
              | Some actor_id, None ->
                  `One (Uuidm.of_string actor_id |> Option.get)
              | None, Some actor_role -> `Entity (R.of_string actor_role)
              | _ -> raise (Invalid_argument "Invalid actor spec")
            in
            let x : Guardian.Authorizer.auth_rule = (actor_spec, action) in
            x :: acc)
          ~init:[]
      in
      let* () = lwt_return_rc rc in
      Lwt.return_ok rv

    let mem_authorizable id =
      let stmt = Sqlite3.prepare db "SELECT * FROM entities WHERE id = ?" in
      let _ =
        let open Sqlite3 in
        bind_values stmt [ Data.TEXT (Uuidm.to_string id) ]
      in
      let _, rv =
        Sqlite3.fold stmt ~f:(fun _acc _row -> Some true) ~init:None
      in
      Option.value rv ~default:false |> Lwt.return_ok

    let grant_roles id roles =
      let id' = Uuidm.to_string id in
      let* roles' =
        let* pre_roles = get_roles id in
        Guardian.Role_set.union pre_roles roles
        |> Guardian.Role_set.to_yojson |> Yojson.Safe.to_string |> Lwt.return_ok
      in
      let* stmt =
        let* mem = mem_authorizable id in
        if mem then
          Lwt.return_ok
            (Sqlite3.prepare db "UPDATE entities SET roles = ? WHERE id = ?")
        else
          Lwt.return_error
            "Cannot grant a role to an authorizable which doesn't exist."
      in
      let () =
        let open Sqlite3 in
        ignore (bind_values stmt Data.[ TEXT roles'; TEXT id' ])
      in
      lwt_return_rc (Sqlite3.step stmt)

    let revoke_roles id roles =
      let id' = Uuidm.to_string id in
      let* roles' =
        let* pre_roles = get_roles id in
        Guardian.Role_set.diff pre_roles roles
        |> Guardian.Role_set.to_yojson |> Yojson.Safe.to_string |> Lwt.return_ok
      in
      let* stmt =
        let* mem = mem_authorizable id in
        if mem then
          Lwt.return_ok
            (Sqlite3.prepare db "UPDATE entities SET roles = ? WHERE id = ?")
        else
          Lwt.return_error
            "Cannot grant a role to an authorizable which doesn't exist."
      in
      let () =
        let open Sqlite3 in
        ignore (bind_values stmt Data.[ TEXT roles'; TEXT id' ])
      in
      lwt_return_rc (Sqlite3.step stmt)

    let set_owner id ~owner =
      let id' = Uuidm.to_string id in
      let owner' = Uuidm.to_string owner in
      let stmt =
        Sqlite3.prepare db "UPDATE entities SET parent = ? WHERE id = ?"
      in
      let () =
        let open Sqlite3 in
        ignore (bind_values stmt Data.[ TEXT owner'; TEXT id' ])
      in
      lwt_return_rc (Sqlite3.step stmt)
  end

  include Guardian.Make_persistence (Backend)
end
