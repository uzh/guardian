module Backend: Ocaml_authorize.Persistence.Backend_store_s = struct
  let db = Sqlite3.db_open "test_db.sqlite3"
  let create_entity ?(id = Uuidm.create `V4) roles =
    let stmt = Sqlite3.prepare db "INSERT INTO entities (id, roles) VALUES (?, ?)" in
    let roles' = Ocaml_authorize.Role_set.to_yojson roles |> Yojson.Safe.to_string in
    let _ =
      let open Sqlite3 in
      bind_values stmt [Data.TEXT (Uuidm.to_string id); Data.TEXT roles']
    in
    try
      ignore(Sqlite3.step stmt) |> Result.ok
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
    let _ =
      let open Sqlite3 in
      let _ = bind_name stmt "id" (Data.TEXT id') in
      step stmt
    in
    match Sqlite3.column_text stmt 0 |> Uuidm.of_string with
    | Some uuid -> Ok(Some uuid)
    | None -> Error("Failed to parse UUID")

  let get_perms spec =
    match spec with
    | `Uniq _uuidm ->
      []
    | `Role _role ->
      []

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
    Ok(ignore(Sqlite3.step stmt))
  let set_owner id ~owner =
    let id' = Uuidm.to_string id in
    let owner' = Uuidm.to_string owner in
    let stmt = Sqlite3.prepare db "UPDATE entities SET owner = :owner WHERE id = :id" in
    let () =
      let open Sqlite3 in
      let _ = bind_name stmt "id" (Data.TEXT id') in
      ignore(bind_name stmt "owner" (Data.TEXT owner'))
    in
    Result.ok (ignore(Sqlite3.step stmt))
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