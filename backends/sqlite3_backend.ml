module Make(R : Ocaml_authorize.Role_s) = struct
  module Ocaml_authorize = Ocaml_authorize.Make(R)

  let ( let* ) = Lwt_result.bind

  (** TODO: generalize this. right now it's fine because this backend should
      really only be used for testing purposes, but in the future it would be
      nice to have actual sqlite3 support. *)
  let db = Sqlite3.db_open "test_db.sqlite3"

  module Backend = struct
    type role = R.t
    type role_set = Ocaml_authorize.Role_set.t
    type 'a authorizable = 'a Ocaml_authorize.Authorizable.t
    type auth_rule = Ocaml_authorize.Authorizer.auth_rule
    type actor_spec = Ocaml_authorize.Authorizer.actor_spec

    type ('rv, 'err) monad = ('rv, 'err) Lwt_result.t
    let lwt_return_rc = function
      | Sqlite3.Rc.OK | DONE | ROW -> Lwt.return_ok()
      | rc -> Lwt.return_error(Sqlite3.Rc.to_string rc)

    let create_authorizable ~id ?(owner : Uuidm.t option) roles : (unit, string) Lwt_result.t =
      let stmt = Sqlite3.prepare db "INSERT INTO entities (id, roles, parent) VALUES (?, ?, ?)" in
      let roles' = Ocaml_authorize.Role_set.to_yojson roles |> Yojson.Safe.to_string in
      let* () =
        let open Sqlite3 in
        bind_values
          stmt
          [ Data.TEXT (Uuidm.to_string id)
          ; Data.TEXT roles'
          ; Data.opt_text (Option.map Uuidm.to_string owner)
          ]
        |> lwt_return_rc
      in
      lwt_return_rc(Sqlite3.step stmt)

    let get_roles id =
      let id' = Uuidm.to_string id in
      let stmt =
        Sqlite3.prepare
          db
          "SELECT roles FROM entities WHERE id = ?"
      in
      let _ =
        let open Sqlite3 in
        bind_values stmt [Data.TEXT id']
      in
      let* () = lwt_return_rc(Sqlite3.step stmt) in
      match String.trim(Sqlite3.column_text stmt 0) with
      | "" -> Lwt.return_ok(Ocaml_authorize.Role_set.empty)
      | coltext ->
        Yojson.Safe.from_string coltext
        |> Ocaml_authorize.Role_set.of_yojson
        |> Lwt.return

    let get_owner id =
      let id' = Uuidm.to_string id in
      let stmt =
        Sqlite3.prepare
          db
          "SELECT parent FROM entities WHERE id = ?"
      in
      let* () =
        let open Sqlite3 in
        let _ = bind_values stmt [Data.TEXT id'] in
        lwt_return_rc(step stmt)
      in
      let raw = Sqlite3.column_text stmt 0 in
      match Uuidm.of_string raw with
      | Some uuid -> Lwt.return_ok(Some uuid)
      | None -> Lwt.return_ok(None)

    let put_perm ((actor, action, target): auth_rule) =
      let action' = Ocaml_authorize.Action.to_string action in
      let stmt =
        let open Sqlite3 in
        match actor, target with
        | `Uniq auuid, `Uniq tuuid ->
          let stmt =
            {|INSERT INTO rules (actor_id, act, target_id)
              VALUES (?, ?, ?)|}
            |> prepare db
          in
          let _ =
            bind_values
              stmt
              Data.[
                TEXT(Uuidm.to_string auuid)
              ; TEXT(action')
              ; TEXT(Uuidm.to_string tuuid)
              ]
          in
          stmt
        | `Uniq auuid, `Role trole ->
          let stmt =
            {|INSERT INTO rules (actor_id, act, target_role)
              VALUES (?, ?, ?)|}
            |> prepare db
          in
          let _ =
            bind_values
              stmt
              Data.[
                TEXT(Uuidm.to_string auuid)
              ; TEXT(action')
              ; TEXT(R.show trole)
              ]
          in
          stmt
        | `Role arole, `Uniq tuuid ->
          let stmt =
            {|INSERT INTO rules (actor_role, act, target_id)
              VALUES (?, ?, ?)|}
            |> prepare db
          in
          let _ =
            bind_values
              stmt
              Data.[
                TEXT(R.show arole)
              ; TEXT(action')
              ; TEXT(Uuidm.to_string tuuid)
              ]
          in
          stmt
        | `Role arole, `Role trole ->
          let stmt =
            {|INSERT INTO rules (actor_role, act, target_role)
              VALUES (?, ?, ?)|}
            |> prepare db
          in
          let _ =
            bind_values
              stmt
              Data.[
                TEXT(R.show arole)
              ; TEXT(action')
              ; TEXT(R.show trole)
              ]
          in
          stmt
      in
      lwt_return_rc (Sqlite3.step stmt)
    let delete_perm ((actor, action, target): Ocaml_authorize.Authorizer.auth_rule) =
      match actor, target with
      | `Uniq aid, `Uniq tid ->
        let stmt = "DELETE FROM rules WHERE actor_id=? AND act=? AND target_id=?" in
        let stmt = Sqlite3.prepare db stmt in
        let* () =
          lwt_return_rc
            Sqlite3.(bind_values
              stmt
              Data.[
                TEXT (Uuidm.to_string aid)
              ; TEXT (Ocaml_authorize.Action.to_string action)
              ; TEXT (Uuidm.to_string tid)])
        in
        lwt_return_rc(Sqlite3.step stmt)
      | `Uniq aid, `Role trole ->
        let stmt = "DELETE FROM rules WHERE actor_id=? AND act=? AND target_role=?" in
        let stmt = Sqlite3.prepare db stmt in
        let* () =
          lwt_return_rc
            Sqlite3.(bind_values
              stmt
              Data.[
                TEXT (Uuidm.to_string aid)
              ; TEXT (Ocaml_authorize.Action.to_string action)
              ; TEXT(R.show trole)])
        in
        lwt_return_rc(Sqlite3.step stmt)
      | `Role arole, `Uniq tid ->
        let stmt = "DELETE FROM rules WHERE actor_role=? AND act=? AND target_id=?" in
        let stmt = Sqlite3.prepare db stmt in
        let* () =
          lwt_return_rc
            Sqlite3.(bind_values
              stmt
              Data.[
                TEXT(R.show arole)
              ; TEXT (Ocaml_authorize.Action.to_string action)
              ; TEXT (Uuidm.to_string tid)])
        in
        lwt_return_rc(Sqlite3.step stmt)
      | `Role arole, `Role trole ->
        let stmt = "DELETE FROM rules WHERE actor_role=? AND act=? AND target_role=?" in
        let stmt = Sqlite3.prepare db stmt in
        let* () =
          lwt_return_rc
            Sqlite3.(bind_values
              stmt
              Data.[
                TEXT(R.show arole)
              ; TEXT (Ocaml_authorize.Action.to_string action)
              ; TEXT(R.show trole)])
        in
        lwt_return_rc(Sqlite3.step stmt)

    let get_perms (spec : actor_spec) =
      let* stmt =
        let open Sqlite3 in
        match spec with
        | `Uniq uuidm ->
          let stmt =
            "SELECT act, actor_id, actor_role FROM rules WHERE target_id = ?"
            |> prepare db
          in
          let* () = lwt_return_rc(bind_values stmt [Data.TEXT(Uuidm.to_string uuidm)]) in
          Lwt.return_ok stmt
        | `Role role ->
          let stmt =
            "SELECT act, actor_id, actor_role FROM rules WHERE target_role = ?"
            |> prepare db
          in
          let () = Printf.printf "Searching for role: %s\n" (R.show role) in
          let* () = lwt_return_rc(bind_values stmt [Data.TEXT (R.show role)]) in
          Lwt.return_ok stmt
      in
      let rc, rv =
        Sqlite3.fold
          stmt
          ~f:(fun acc row ->
              let action =
                row.(0)
                |> Sqlite3.Data.to_string_exn
                |> Ocaml_authorize.Action.of_string
              in
              let (actor_spec : Ocaml_authorize.Authorizer.actor_spec) =
                match Sqlite3.Data.(to_string row.(1), to_string row.(2)) with
                | Some actor_id, None ->
                  `Uniq(Uuidm.of_string actor_id |> Option.get)
                | None, Some actor_role ->
                  `Role(R.of_string actor_role)
                | _ ->
                  raise(Invalid_argument "Invalid actor spec")
              in
              let x: Ocaml_authorize.Authorizer.auth_rule = actor_spec, action, spec in
              x :: acc
            )
          ~init:[]
      in
      let* () = lwt_return_rc rc in
      Lwt.return_ok rv

    let mem_authorizable id =
      let stmt =
        Sqlite3.prepare
          db
          "SELECT * FROM entities WHERE id = ?"
      in
      let _ =
        let open Sqlite3 in
        bind_values stmt [Data.TEXT (Uuidm.to_string id)]
      in
      let (_, rv) = Sqlite3.fold stmt ~f:(fun _acc _row -> Some true) ~init:None in
      Option.value rv ~default:false
      |> Lwt.return_ok

    let grant_roles id roles =
      let id' = Uuidm.to_string id in
      let* roles' =
        let* pre_roles = get_roles id in
        Ocaml_authorize.Role_set.union pre_roles roles
        |> Ocaml_authorize.Role_set.to_yojson
        |> Yojson.Safe.to_string
        |> Lwt.return_ok
      in
      let* stmt =
        let* mem = mem_authorizable id in
        if mem
        then Lwt.return_ok(Sqlite3.prepare db "UPDATE entities SET roles = ? WHERE id = ?")
        else Lwt.return_error "Cannot grant a role to an authorizable which doesn't exist."
      in
      let () =
        let open Sqlite3 in
        ignore(bind_values stmt Data.[TEXT roles'; TEXT id'])
      in
      lwt_return_rc(Sqlite3.step stmt)
    let set_owner id ~owner =
      let id' = Uuidm.to_string id in
      let owner' = Uuidm.to_string owner in
      let stmt = Sqlite3.prepare db "UPDATE entities SET parent = ? WHERE id = ?" in
      let () =
        let open Sqlite3 in
        ignore(bind_values stmt Data.[TEXT owner'; TEXT id'])
      in
      lwt_return_rc(Sqlite3.step stmt)
  end

  include Ocaml_authorize.Make_persistence(Backend)
end