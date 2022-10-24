(** TODO: optional [ctx] arguments are ignored by sqlite backend *)
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
    ;;

    let create_authorizable ?ctx ~id ?(owner : Uuidm.t option) roles
      : (unit, string) Lwt_result.t
      =
      let open Sqlite3 in
      let (_ : (string * string) list option) = ctx in
      let stmt =
        prepare
          db
          {sql|INSERT INTO guardian_entities (id, roles, parent) VALUES (?, ?, ?)|sql}
      in
      let roles' = Guardian.Role_set.to_yojson roles |> Yojson.Safe.to_string in
      let* () =
        bind_values
          stmt
          [ Data.TEXT (Uuidm.to_string id)
          ; Data.TEXT roles'
          ; Data.opt_text (CCOption.map Uuidm.to_string owner)
          ]
        |> lwt_return_rc
      in
      lwt_return_rc (step stmt)
    ;;

    let find_roles ?ctx id =
      let open Sqlite3 in
      let (_ : (string * string) list option) = ctx in
      let id' = Uuidm.to_string id in
      let stmt =
        prepare db {sql|SELECT roles FROM guardian_entities WHERE id = ?|sql}
      in
      let (_ : Rc.t) = bind_values stmt [ Data.TEXT id' ] in
      let* () = lwt_return_rc (step stmt) in
      match String.trim (column_text stmt 0) with
      | "" -> Lwt.return_ok Guardian.Role_set.empty
      | coltext ->
        Yojson.Safe.from_string coltext
        |> Guardian.Role_set.of_yojson
        |> Lwt.return
    ;;

    let find_owner ?ctx id =
      let open Sqlite3 in
      let (_ : (string * string) list option) = ctx in
      let id' = Uuidm.to_string id in
      let stmt =
        prepare db {sql|SELECT parent FROM guardian_entities WHERE id = ?|sql}
      in
      let* () =
        let (_ : Rc.t) = bind_values stmt [ Data.TEXT id' ] in
        lwt_return_rc (step stmt)
      in
      let raw = column_text stmt 0 in
      match Uuidm.of_string raw with
      | Some uuid -> Lwt.return_ok (Some uuid)
      | None -> Lwt.return_ok None
    ;;

    let save_rule ?ctx ((actor, action, target) : auth_rule) =
      let open Sqlite3 in
      let (_ : (string * string) list option) = ctx in
      let action' = Guardian.Action.to_string action in
      let stmt =
        match actor, target with
        | `One auuid, `One tuuid ->
          let stmt =
            {sql|
              INSERT INTO guardian_rules (actor_id, act, target_id)
              VALUES (?, ?, ?)
            |sql}
            |> prepare db
          in
          let _ =
            bind_values
              stmt
              Data.
                [ TEXT (Uuidm.to_string auuid)
                ; TEXT action'
                ; TEXT (Uuidm.to_string tuuid)
                ]
          in
          stmt
        | `One auuid, `Entity trole ->
          let stmt =
            {sql|
              INSERT INTO guardian_rules (actor_id, act, target_role)
              VALUES (?, ?, ?)
            |sql}
            |> prepare db
          in
          let _ =
            bind_values
              stmt
              Data.
                [ TEXT (Uuidm.to_string auuid)
                ; TEXT action'
                ; TEXT (R.show trole)
                ]
          in
          stmt
        | `Entity arole, `One tuuid ->
          let stmt =
            {sql|
              INSERT INTO guardian_rules (actor_role, act, target_id)
              VALUES (?, ?, ?)
            |sql}
            |> prepare db
          in
          let _ =
            bind_values
              stmt
              Data.
                [ TEXT (R.show arole)
                ; TEXT action'
                ; TEXT (Uuidm.to_string tuuid)
                ]
          in
          stmt
        | `Entity arole, `Entity trole ->
          let stmt =
            {sql|
              INSERT INTO guardian_rules (actor_role, act, target_role)
              VALUES (?, ?, ?)
            |sql}
            |> prepare db
          in
          let _ =
            bind_values
              stmt
              Data.[ TEXT (R.show arole); TEXT action'; TEXT (R.show trole) ]
          in
          stmt
      in
      lwt_return_rc (step stmt)
    ;;

    let delete_rule
      ?ctx
      ((actor, action, target) : Guardian.Authorizer.auth_rule)
      =
      let open Sqlite3 in
      let (_ : (string * string) list option) = ctx in
      match actor, target with
      | `One aid, `One tid ->
        let stmt =
          {sql|
            DELETE FROM guardian_rules WHERE actor_id = ? AND act = ? AND target_id = ?
          |sql}
        in
        let stmt = prepare db stmt in
        let* () =
          lwt_return_rc
            (bind_values
               stmt
               Data.
                 [ TEXT (Uuidm.to_string aid)
                 ; TEXT (Guardian.Action.to_string action)
                 ; TEXT (Uuidm.to_string tid)
                 ])
        in
        lwt_return_rc (step stmt)
      | `One aid, `Entity trole ->
        let stmt =
          {sql|
            DELETE FROM guardian_rules WHERE actor_id = ? AND act = ? AND target_role = ?
          |sql}
        in
        let stmt = prepare db stmt in
        let* () =
          lwt_return_rc
            (bind_values
               stmt
               Data.
                 [ TEXT (Uuidm.to_string aid)
                 ; TEXT (Guardian.Action.to_string action)
                 ; TEXT (R.show trole)
                 ])
        in
        lwt_return_rc (step stmt)
      | `Entity arole, `One tid ->
        let stmt =
          {sql|
            DELETE FROM guardian_rules WHERE actor_role = ? AND act = ? AND target_id = ?
          |sql}
        in
        let stmt = prepare db stmt in
        let* () =
          lwt_return_rc
            (bind_values
               stmt
               Data.
                 [ TEXT (R.show arole)
                 ; TEXT (Guardian.Action.to_string action)
                 ; TEXT (Uuidm.to_string tid)
                 ])
        in
        lwt_return_rc (step stmt)
      | `Entity arole, `Entity trole ->
        let stmt =
          {sql|
            DELETE FROM guardian_rules WHERE actor_role = ? AND act = ? AND target_role = ?
          |sql}
        in
        let stmt = prepare db stmt in
        let* () =
          lwt_return_rc
            (bind_values
               stmt
               Data.
                 [ TEXT (R.show arole)
                 ; TEXT (Guardian.Action.to_string action)
                 ; TEXT (R.show trole)
                 ])
        in
        lwt_return_rc (step stmt)
    ;;

    let find_rules ?ctx (spec : actor_spec) =
      let open Sqlite3 in
      let (_ : (string * string) list option) = ctx in
      let* stmt =
        match spec with
        | `One uuidm ->
          let stmt =
            {sql|
              SELECT act, actor_id, actor_role FROM guardian_rules WHERE target_id = ?
            |sql}
            |> prepare db
          in
          let* () =
            lwt_return_rc
              (bind_values stmt [ Data.TEXT (Uuidm.to_string uuidm) ])
          in
          Lwt.return_ok stmt
        | `Entity role ->
          let stmt =
            {sql|
              SELECT act, actor_id, actor_role FROM guardian_rules WHERE target_role = ?
            |sql}
            |> prepare db
          in
          let () = Printf.printf "Searching for role: %s\n" (R.show role) in
          let* () =
            lwt_return_rc (bind_values stmt [ Data.TEXT (R.show role) ])
          in
          Lwt.return_ok stmt
      in
      let rc, rv =
        fold
          stmt
          ~f:(fun acc row ->
            let action =
              row.(0) |> Data.to_string_exn |> Guardian.Action.of_string
            in
            let (actor_spec : Guardian.Authorizer.actor_spec) =
              match Data.(to_string row.(1), to_string row.(2)) with
              | Some actor_id, None ->
                `One
                  (Uuidm.of_string actor_id
                  |> CCOption.get_exn_or "Malformatted UUID!")
              | None, Some actor_role -> `Entity (R.of_string actor_role)
              | _ -> raise (Invalid_argument "Invalid actor spec")
            in
            let x : Guardian.Authorizer.auth_rule = actor_spec, action, spec in
            x :: acc)
          ~init:[]
      in
      let* () = lwt_return_rc rc in
      Lwt.return_ok rv
    ;;

    let mem_authorizable ?ctx id =
      let open Sqlite3 in
      let (_ : (string * string) list option) = ctx in
      let stmt =
        prepare db {sql|SELECT * FROM guardian_entities WHERE id = ?|sql}
      in
      let (_ : Rc.t) = bind_values stmt [ Data.TEXT (Uuidm.to_string id) ] in
      let (_ : Rc.t), rv =
        fold stmt ~f:(fun _acc _row -> Some true) ~init:None
      in
      rv |> CCOption.get_or ~default:false |> Lwt.return_ok
    ;;

    let grant_roles ?ctx id roles =
      let open Sqlite3 in
      let (_ : (string * string) list option) = ctx in
      let id' = Uuidm.to_string id in
      let* roles' =
        let* pre_roles = find_roles id in
        Guardian.Role_set.union pre_roles roles
        |> Guardian.Role_set.to_yojson
        |> Yojson.Safe.to_string
        |> Lwt.return_ok
      in
      let* stmt =
        let* mem = mem_authorizable id in
        if mem
        then
          Lwt.return_ok
            (prepare
               db
               {sql|UPDATE guardian_entities SET roles = ? WHERE id = ?|sql})
        else
          Lwt.return_error
            "Cannot grant a role to an authorizable which doesn't exist."
      in
      let () = ignore (bind_values stmt Data.[ TEXT roles'; TEXT id' ]) in
      lwt_return_rc (step stmt)
    ;;

    let revoke_roles ?ctx id roles =
      let open Sqlite3 in
      let (_ : (string * string) list option) = ctx in
      let id' = Uuidm.to_string id in
      let* roles' =
        let* pre_roles = find_roles id in
        Guardian.Role_set.diff pre_roles roles
        |> Guardian.Role_set.to_yojson
        |> Yojson.Safe.to_string
        |> Lwt.return_ok
      in
      let* stmt =
        let* mem = mem_authorizable id in
        if mem
        then
          Lwt.return_ok
            (prepare
               db
               {sql|UPDATE guardian_entities SET roles = ? WHERE id = ?|sql})
        else
          Lwt.return_error
            "Cannot grant a role to an authorizable which doesn't exist."
      in
      let () = ignore (bind_values stmt Data.[ TEXT roles'; TEXT id' ]) in
      lwt_return_rc (step stmt)
    ;;

    let save_owner ?ctx id ~owner =
      let open Sqlite3 in
      let (_ : (string * string) list option) = ctx in
      let id' = Uuidm.to_string id in
      let owner' = Uuidm.to_string owner in
      let stmt =
        prepare
          db
          {sql|UPDATE guardian_entities SET parent = ? WHERE id = ?|sql}
      in
      let () = ignore (bind_values stmt Data.[ TEXT owner'; TEXT id' ]) in
      lwt_return_rc (step stmt)
    ;;
  end

  include Guardian.Make_persistence (Backend)
end
