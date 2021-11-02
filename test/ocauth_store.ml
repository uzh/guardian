let db = Sqlite3.db_open ~mode:`NO_CREATE "test_db.sqlite3"

module Backend: Ocaml_authorize.Persistence.Backend_store_s = struct
  let return_rc = function
    | Sqlite3.Rc.OK -> Ok()
    | rc -> Error(Sqlite3.Rc.to_string rc)

  let create_entity ?(id = Uuidm.create `V4) roles =
    let stmt = Sqlite3.prepare db "INSERT INTO entities (id, roles) VALUES (?, ?)" in
    let roles' = Ocaml_authorize.Role_set.to_yojson roles |> Yojson.Safe.to_string in
    let _ =
      let open Sqlite3 in
      bind_values stmt [Data.TEXT (Uuidm.to_string id); Data.TEXT roles']
    in
    try
      match Sqlite3.finalize stmt with
      | Sqlite3.Rc.OK -> Ok ()
      | rc -> Error("Bad return: " ^ Sqlite3.Rc.to_string rc)
    with Sqlite3.SqliteError err ->
      Result.error err

  let get_roles id =
    let id' = Uuidm.to_string id in
    let stmt =
      Sqlite3.prepare
        db
        "SELECT roles FROM entities WHERE id = :id"
    in
    let _ =
      let open Sqlite3 in
      bind_name stmt "id" (Data.TEXT id')
    in
    let (_, rv) =
      Sqlite3.fold
        stmt
        ~f:(fun acc row ->
            let ( let* ) = Result.bind in
            let* acc' = acc in
            match Sqlite3.Data.to_string row.(0) with
            | Some x -> 
              let* x' = Ocaml_authorize.Role_set.of_yojson (Yojson.Safe.from_string x) in
              Ocaml_authorize.Role_set.union acc' x'
              |> Result.ok
            | None -> acc)
        ~init:(Ok Ocaml_authorize.Role_set.empty)
    in
    rv
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
    match Sqlite3.finalize stmt with
    | Sqlite3.Rc.OK -> Ok ()
    | rc -> Error(Sqlite3.Rc.to_string rc)

  let get_perms spec =
    let stmt =
      let open Sqlite3 in
      match spec with
      | `Uniq uuidm ->
        let stmt =
          "SELECT (act, actor_id, actor_role) FROM rules WHERE target_id = ?"
          |> prepare db
        in
        let _ = bind_values stmt [Data.TEXT(Uuidm.to_string uuidm)] in
        stmt
      | `Role role ->
        let stmt =
          "SELECT (act, actor_id, actor_role) FROM rules WHERE target_role = ?"
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

  let grant_roles id roles =
    let id' = Uuidm.to_string id in
    let roles': Yojson.Safe.t =
      let pre_roles = Result.value (get_roles id) ~default:Ocaml_authorize.Role_set.empty in
      Ocaml_authorize.Role_set.union pre_roles roles
      |> Ocaml_authorize.Role_set.to_yojson
    in
    let roles'' = Yojson.Safe.to_string roles' in
    let stmt = Sqlite3.prepare db "UPDATE entities SET roles = :roles WHERE id = :id" in
    let () =
      let open Sqlite3 in
      let _ = bind_name stmt "id" (Data.TEXT id') in
      ignore(bind_name stmt "roles" (Data.TEXT roles''))
    in
    return_rc(Sqlite3.finalize stmt)
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
end

include Ocaml_authorize.Persistence.Make(Backend)