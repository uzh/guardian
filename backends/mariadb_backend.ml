open Lwt.Infix
open Caqti_request.Infix

module Make
    (R : Guardian.Role_s) (CONFIG : sig
      val connection_string : string
    end)
    () =
struct
  module Guardian = Guardian.Make (R)

  module Db =
    (val Caqti_lwt.connect (Uri.of_string CONFIG.connection_string)
         >>= Caqti_lwt.or_fail |> Lwt_main.run)

  let ( let* ) = Lwt_result.bind

  include Guardian.Make_persistence (struct
    type role = R.t
    type role_set = Guardian.Role_set.t
    type 'a authorizable = 'a Guardian.Authorizable.t
    type auth_rule = Guardian.Authorizer.auth_rule
    type actor_spec = Guardian.Authorizer.actor_spec
    type ('rv, 'err) monad = ('rv, 'err) Lwt_result.t

    let get_roles id : (Guardian.Role_set.t, string) Lwt_result.t =
      let caqti =
        (Caqti_type.string ->! Caqti_type.string)
        @@ "SELECT roles FROM entities WHERE id = ?"
      in
      match%lwt Db.find caqti (Uuidm.to_string id) with
      | Ok s ->
          Lwt.return (Guardian.Role_set.of_yojson (Yojson.Safe.from_string s))
      | Error err -> Lwt.return_error (Caqti_error.show err)

    let get_perms target_spec :
        (Guardian.Authorizer.auth_rule list, string) Lwt_result.t =
      let%lwt res =
        match target_spec with
        | `One uuid ->
            let caqti =
              (Caqti_type.string
              ->* Caqti_type.(tup3 string (option string) (option string)))
              @@ "SELECT act, actor_id, actor_role FROM rules WHERE target_id \
                  = ?"
            in
            Db.collect_list caqti (Uuidm.to_string uuid)
        | `Entity role ->
            let caqti =
              (Caqti_type.string
              ->* Caqti_type.(tup3 string (option string) (option string)))
              @@ "SELECT act, actor_id, actor_role FROM rules WHERE \
                  target_role = ?"
            in
            Db.collect_list caqti (R.show role)
      in
      match res with
      | Ok s ->
          List.map
            (fun (act, actor_id, actor_role) : Guardian.Authorizer.auth_rule ->
              let act = Guardian.Action.of_string act in
              match (actor_id, Option.map R.of_string actor_role) with
              | Some id, None -> (
                  match Uuidm.of_string id with
                  | Some id' -> (`One id', act, target_spec)
                  | None -> raise (Failure ("Invalid UUID: " ^ id)))
              | None, Some role -> (`Entity role, act, target_spec)
              | Some _, Some _ | None, None ->
                  raise
                    (Failure
                       "Either both actor fields were occupied, or both were \
                        not."))
            s
          |> Lwt.return_ok
      | Error err -> Lwt.return_error (Caqti_error.show err)

    let act_on_perm query (actor, act, target) =
      let spec_to_str = function
        | `One s -> Uuidm.to_string s
        | `Entity s -> R.show s
      in
      let actor' = spec_to_str actor in
      let act' = Guardian.Action.to_string act in
      let target' = spec_to_str target in
      let caqti =
        (Caqti_type.(tup3 string string string) ->. Caqti_type.unit) @@ query
      in
      match%lwt Db.exec caqti (actor', act', target') with
      | Ok () -> Lwt.return_ok ()
      | Error err -> Lwt.return_error (Caqti_error.show err)

    let put_perm (auth_rule : auth_rule) : (unit, string) Lwt_result.t =
      let query =
        match auth_rule with
        | `One _, _, `One _ ->
            "INSERT INTO rules (actor_id, act, target_id) VALUES (?, ?, ?)"
        | `One _, _, `Entity _ ->
            "INSERT INTO rules (actor_id, act, target_role) VALUES (?, ?, ?)"
        | `Entity _, _, `One _ ->
            "INSERT INTO rules (actor_role, act, target_id) VALUES (?, ?, ?)"
        | `Entity _, _, `Entity _ ->
            "INSERT INTO rules (actor_role, act, target_role) VALUES (?, ?, ?)"
      in
      act_on_perm query auth_rule

    let delete_perm auth_rule =
      let query =
        match auth_rule with
        | `One _, _, `One _ ->
            "DELETE FROM rules WHERE actor_id = ? AND act = ? AND target_id = ?"
        | `One _, _, `Entity _ ->
            "DELETE FROM rules WHERE actor_id = ? AND act = ? AND target_role \
             = ?"
        | `Entity _, _, `One _ ->
            "DELETE FROM rules WHERE actor_role = ? AND act = ? AND target_id \
             = ?"
        | `Entity _, _, `Entity _ ->
            "DELETE FROM rules WHERE actor_role = ? AND act = ? AND \
             target_role = ?"
      in
      act_on_perm query auth_rule

    let grant_roles uuid roles =
      let open Guardian in
      let* pre_roles = get_roles uuid in
      let roles' = Role_set.union roles pre_roles in
      if Role_set.(cardinal roles' > cardinal pre_roles) then
        let caqti =
          (Caqti_type.(tup2 string string) ->. Caqti_type.unit)
          @@ "UPDATE entities SET roles = ? WHERE id = ?"
        in
        let roles'' = Yojson.Safe.to_string (Role_set.to_yojson roles') in
        match%lwt Db.exec caqti (roles'', Uuidm.to_string uuid) with
        | Ok () -> Lwt.return_ok ()
        | Error err -> Lwt.return_error (Caqti_error.show err)
      else Lwt.return_ok ()

    let revoke_roles uuid roles =
      let open Guardian in
      let* pre_roles = get_roles uuid in
      let roles' = Role_set.diff pre_roles roles in
      let caqti =
        (Caqti_type.(tup2 string string) ->. Caqti_type.unit)
        @@ "UPDATE entities SET roles = ? WHERE id = ?"
      in
      let roles'' = Yojson.Safe.to_string (Role_set.to_yojson roles') in
      match%lwt Db.exec caqti (roles'', Uuidm.to_string uuid) with
      | Ok () -> Lwt.return_ok ()
      | Error err -> Lwt.return_error (Caqti_error.show err)

    let create_authorizable ~id ?owner roles =
      let caqti =
        (Caqti_type.(tup3 string string (option string)) ->. Caqti_type.unit)
        @@ "INSERT INTO entities (id, roles, parent) VALUES (?, ?, ?)"
      in
      let roles' = Guardian.Role_set.to_yojson roles |> Yojson.Safe.to_string in
      let owner' = Option.map Uuidm.to_string owner in
      match%lwt Db.exec caqti (Uuidm.to_string id, roles', owner') with
      | Ok _ -> Lwt.return_ok ()
      | Error err -> Lwt.return_error (Caqti_error.show err)

    let mem_authorizable id =
      let caqti =
        (Caqti_type.string ->? Caqti_type.string)
        @@ "SELECT roles FROM entities WHERE id = ?"
      in
      match%lwt Db.find_opt caqti (Uuidm.to_string id) with
      | Ok (Some _) -> Lwt.return_ok true
      | Ok None -> Lwt.return_ok false
      | Error err -> Lwt.return_error (Caqti_error.show err)

    let get_owner id =
      let caqti =
        (Caqti_type.string ->! Caqti_type.(option string))
        @@ "SELECT parent FROM entities WHERE id = ?"
      in
      match%lwt Db.find caqti (Uuidm.to_string id) with
      | Ok s -> Lwt.return_ok (Option.bind s Uuidm.of_string)
      | Error err -> Lwt.return_error (Caqti_error.show err)

    let set_owner id ~owner =
      let caqti =
        (Caqti_type.(tup2 string string) ->. Caqti_type.unit)
        @@ "UPDATE entities SET parent = ? WHERE id = ?"
      in
      match%lwt Db.exec caqti Uuidm.(to_string owner, to_string id) with
      | Ok _ -> Lwt.return_ok ()
      | Error err -> Lwt_result.fail (Caqti_error.show err)
  end)
end
