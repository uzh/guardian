let db = Sqlite3.db_open "test_db.sqlite3"

let ( let* ) = Result.bind

module Backend: Ocaml_authorize.Persistence.Backend_store_s = struct
  let return_rc ?(value = ()) = function
    | Sqlite3.Rc.OK | DONE | ROW -> Ok value
    | rc -> Error(Sqlite3.Rc.to_string rc)

  let create_entity ?(id = Uuidm.create `V4) roles =
    let stmt = Sqlite3.prepare db "INSERT INTO entities (id, roles) VALUES (?, ?)" in
    let roles' = Ocaml_authorize.Role_set.to_yojson roles |> Yojson.Safe.to_string in
    let _ =
      let open Sqlite3 in
      bind_values stmt [Data.TEXT (Uuidm.to_string id); Data.TEXT roles']
    in
    return_rc (Sqlite3.step stmt)

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
    let* () = return_rc(Sqlite3.step stmt) in
    match String.trim(Sqlite3.column_text stmt 0) with
    | "" -> Ok(Ocaml_authorize.Role_set.empty)
    | coltext ->
      Yojson.Safe.from_string coltext
      |> Ocaml_authorize.Role_set.of_yojson
  let get_owner id =
    let id' = Uuidm.to_string id in
    let stmt =
      Sqlite3.prepare
        db
        "SELECT owner FROM entities WHERE id = :id"
    in
    let ( let* ) = Result.bind in
    let* () =
      let open Sqlite3 in
      let _ = bind_name stmt "id" (Data.TEXT id') in
      return_rc(finalize stmt)
    in
    match Sqlite3.column_text stmt 0 |> Uuidm.of_string with
    | Some uuid -> Ok(Some uuid)
    | None -> Error("Failed to parse UUID")

  let put_perm (actor, action, target) =
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
            ; TEXT(trole)
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
              TEXT(arole)
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
              TEXT(arole)
            ; TEXT(action')
            ; TEXT(trole)
            ]
        in
        stmt
    in
    return_rc (Sqlite3.step stmt)

  let get_perms spec =
    let stmt =
      let open Sqlite3 in
      match spec with
      | `Uniq uuidm ->
        let stmt =
          "SELECT act, actor_id, actor_role FROM rules WHERE target_id = ?"
          |> prepare db
        in
        let _ = bind_values stmt [Data.TEXT(Uuidm.to_string uuidm)] in
        stmt
      | `Role role ->
        let stmt =
          "SELECT act, actor_id, actor_role FROM rules WHERE target_role = ?"
          |> prepare db
        in
        let _ = bind_values stmt [Data.TEXT role] in
        stmt
    in
    let _rc, rv =
      Sqlite3.fold stmt
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
                `Role actor_role
              | _ ->
                raise(Invalid_argument "Invalid actor spec")
            in
            let x: Ocaml_authorize.Authorizer.auth_rule = actor_spec, action, spec in
            x :: acc
          )
        ~init:[]
    in
    rv
  let mem_entity id =
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
  let grant_roles id roles =
    let id' = Uuidm.to_string id in
    let* roles' =
      let* pre_roles = get_roles id in
      Ocaml_authorize.Role_set.union pre_roles roles
      |> Ocaml_authorize.Role_set.to_yojson
      |> Yojson.Safe.to_string
      |> Result.ok
    in
    let stmt =
      if mem_entity id
      then Sqlite3.prepare db "UPDATE entities SET roles = ? WHERE id = ?"
      else Sqlite3.prepare db "INSERT INTO entities (roles, id) VALUES (?, ?)"
    in
    let () =
      let open Sqlite3 in
      ignore(bind_values stmt Data.[TEXT roles'; TEXT id'])
    in
    return_rc(Sqlite3.step stmt)
  let set_owner id ~owner =
    let id' = Uuidm.to_string id in
    let owner' = Uuidm.to_string owner in
    let stmt = Sqlite3.prepare db "UPDATE entities SET owner = :owner WHERE id = :id" in
    let () =
      let open Sqlite3 in
      let _ = bind_name stmt "id" (Data.TEXT id') in
      ignore(bind_name stmt "owner" (Data.TEXT owner'))
    in
    return_rc(Sqlite3.finalize stmt)
end

include Ocaml_authorize.Persistence.Make(Backend)