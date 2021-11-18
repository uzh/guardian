open Lwt.Infix

module Make(CONFIG : sig val connection_string : string end) () : Ocaml_authorize.Persistence.S = struct
  module Db =
    (val Caqti_lwt.connect
      (Uri.of_string CONFIG.connection_string)
      >>= Caqti_lwt.or_fail
      |> Lwt_main.run)

  let ( let* ) = Lwt_result.bind

  include Ocaml_authorize.Persistence.Make(struct
    let get_roles id : (Ocaml_authorize.Role_set.t, string) Lwt_result.t =
      let caqti =
        Caqti_request.find
          Caqti_type.string
          Caqti_type.string
          "SELECT roles FROM entities WHERE id = ?"
      in
      match%lwt Db.find caqti (Uuidm.to_string id) with
      | Ok s -> Lwt.return(Ocaml_authorize.Role_set.of_yojson(Yojson.Safe.from_string s))
      | Error err -> Lwt.return_error(Caqti_error.show err)

    let get_perms target_spec : (Ocaml_authorize.Authorizer.auth_rule list, string) Lwt_result.t =
      let%lwt res =
        match target_spec with
        | `Uniq uuid ->
          let caqti =
            Caqti_request.collect
              Caqti_type.string
              Caqti_type.(tup3 string (option string) (option string))
              "SELECT act, actor_id, actor_role FROM rules WHERE target_id = ?"
          in
          Db.collect_list caqti (Uuidm.to_string uuid)
        | `Role role ->
          let caqti =
            Caqti_request.collect
              Caqti_type.string
              Caqti_type.(tup3 string (option string) (option string))
              "SELECT act, actor_id, actor_role FROM rules WHERE target_role = ?"
          in
          Db.collect_list caqti role
      in
      match res with
      | Ok s ->
        List.map
          (fun (act, actor_id, actor_role): Ocaml_authorize.Authorizer.auth_rule ->
            let act = Ocaml_authorize.Action.of_string act in
            match actor_id, actor_role with
            | Some id, None ->
              begin match Uuidm.of_string id with
              | Some id' -> (`Uniq id'), act, target_spec
              | None -> raise(Failure("Invalid UUID: " ^ id))
              end
            | None, Some role ->
              `Role role, act, target_spec
            | Some _, Some _
            | None, None ->
              raise(Failure "Either both actor fields were occupied, or both were not.")
          )
          s
        |> Lwt.return_ok
      | Error err -> Lwt.return_error(Caqti_error.show err)
    
    let act_on_perm query (actor, act, target) =
      let spec_to_str = function
        | `Uniq s -> Uuidm.to_string s
        | `Role s -> s
      in
      let actor' = spec_to_str actor in
      let act' = Ocaml_authorize.Action.to_string act in
      let target' = spec_to_str target in
      let caqti = Caqti_request.exec Caqti_type.(tup3 string string string) query in
      match%lwt Db.exec caqti (actor', act', target') with
      | Ok() -> Lwt.return_ok()
      | Error err -> Lwt.return_error(Caqti_error.show err)

    let put_perm auth_rule : (unit, string) Lwt_result.t =
      let query =
        match auth_rule with
        | `Uniq _, _, `Uniq _ ->
          "INSERT INTO rules (actor_id, act, target_id) VALUES (?, ?, ?)"
        | `Uniq _, _, `Role _ ->
          "INSERT INTO rules (actor_id, act, target_role) VALUES (?, ?, ?)"
        | `Role _, _, `Uniq _ ->
          "INSERT INTO rules (actor_role, act, target_id) VALUES (?, ?, ?)"
        | `Role _, _, `Role _ ->
          "INSERT INTO rules (actor_role, act, target_role) VALUES (?, ?, ?)"
      in
      act_on_perm query auth_rule

    let delete_perm auth_rule =
      let query =
        match auth_rule with
        | `Uniq _, _, `Uniq _ ->
          "DELETE FROM rules WHERE actor_id = ? AND act = ? AND target_id = ?"
        | `Uniq _, _, `Role _ ->
          "DELETE FROM rules WHERE actor_id = ? AND act = ? AND target_role = ?"
        | `Role _, _, `Uniq _ ->
          "DELETE FROM rules WHERE actor_role = ? AND act = ? AND target_id = ?"
        | `Role _, _, `Role _ ->
          "DELETE FROM rules WHERE actor_role = ? AND act = ? AND target_role = ?"
      in
      act_on_perm query auth_rule

    let grant_roles uuid roles =
      let open Ocaml_authorize in
      let* pre_roles = get_roles uuid in
      let roles' = Role_set.union roles pre_roles in
      if Role_set.(cardinal roles' > cardinal pre_roles)
      then
        let caqti =
          Caqti_request.exec
            Caqti_type.(tup2 string string)
            "UPDATE entities SET roles = ? WHERE id = ?"
        in
        let roles'' = Yojson.Safe.to_string(Role_set.to_yojson roles') in
        match%lwt Db.exec caqti (roles'', Uuidm.to_string uuid) with
        | Ok () -> Lwt.return_ok()
        | Error err -> Lwt.return_error(Caqti_error.show err)
      else
        Lwt.return_ok()

    let create_authorizable ~id ?owner roles =
      let caqti =
        Caqti_request.exec
          Caqti_type.(tup3 string string (option string))
          "INSERT INTO entities (id, roles, parent) VALUES (?, ?, ?)"
      in
      let roles' = Ocaml_authorize.Role_set.to_yojson roles |> Yojson.Safe.to_string in
      let owner' = Option.map Uuidm.to_string owner in
      match%lwt Db.exec caqti (Uuidm.to_string id, roles', owner') with
      | Ok _ -> Lwt.return_ok()
      | Error err -> Lwt.return_error(Caqti_error.show err)

    let mem_authorizable id =
      let caqti =
        Caqti_request.find_opt
          Caqti_type.string
          Caqti_type.string
          "SELECT roles FROM entities WHERE id = ?"
      in
      match%lwt Db.find_opt caqti (Uuidm.to_string id) with
      | Ok(Some _) -> Lwt.return_true
      | Ok None -> Lwt.return_false
      | Error err -> raise(Failure(Caqti_error.show err))

    let get_owner id =
      let caqti =
        Caqti_request.find
          Caqti_type.string
          Caqti_type.(option string)
          "SELECT parent FROM entities WHERE id = ?"
      in
      match%lwt Db.find caqti (Uuidm.to_string id) with
      | Ok s -> Lwt.return_ok(Option.bind s Uuidm.of_string)
      | Error err -> Lwt.return_error(Caqti_error.show err)

    let set_owner id ~owner =
      let caqti =
        Caqti_request.exec
          Caqti_type.(tup2 string string)
          "UPDATE entities SET parent = ? WHERE id = ?"
      in
      match%lwt Db.exec caqti Uuidm.(to_string owner, to_string id) with
      | Ok _ -> Lwt.return_ok()
      | Error err -> Lwt_result.fail(Caqti_error.show err)
    end)
end

let make (connection_string : string) =
  let module RV = Make(struct let connection_string = connection_string end)() in
  let rv = (module RV : Ocaml_authorize.Persistence.S) in
  rv
