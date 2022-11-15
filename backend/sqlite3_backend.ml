exception Exception of string

let get_or_exn m = m |> CCResult.get_or_failwith |> Lwt.return

(** TODO: optional [ctx] arguments are ignored by sqlite backend *)
module Make (A : Guardian.RoleSig) (T : Guardian.RoleSig) = struct
  module Guardian = Guardian.Make (A) (T)
  module Uuid = Guardian.Uuid

  (** TODO: generalize this. right now it's fine because this backend should
      really only be used for testing purposes, but in the future it would be
      nice to have actual sqlite3 support. *)
  module Set = Guardian.ActorRoleSet

  let db = Sqlite3.db_open "test_db.sqlite3"

  module Backend = struct
    type 'a authorizable = 'a Guardian.Authorizable.t
    type 'b authorizable_target = 'b Guardian.AuthorizableTarget.t
    type role = A.t
    type auth_rule = Guardian.Authorizer.auth_rule
    type actor_role_set = Guardian.ActorRoleSet.t
    type actor_spec = Guardian.Authorizer.actor_spec
    type target_role_set = Guardian.TargetRoleSet.t
    type target_spec = Guardian.Authorizer.target_spec
    type ('rv, 'err) monad = ('rv, 'err) Lwt_result.t

    let lwt_return_rc = function
      | Sqlite3.Rc.OK | DONE | ROW -> Lwt.return_ok ()
      | rc -> Lwt.return_error (Sqlite3.Rc.to_string rc)
    ;;

    module Actor = struct
      let create_authorizable ?ctx ~id ?(owner : Uuid.Actor.t option) roles
        : (unit, string) Lwt_result.t
        =
        let open Lwt_result.Syntax in
        let open Sqlite3 in
        let (_ : (string * string) list option) = ctx in
        let stmt =
          prepare
            db
            {sql|INSERT INTO guardian_entities (id, roles, parent) VALUES (?, ?, ?)|sql}
        in
        let roles' = Set.to_yojson roles |> Yojson.Safe.to_string in
        let* () =
          bind_values
            stmt
            [ Data.TEXT (Uuid.Actor.to_string id)
            ; Data.TEXT roles'
            ; Data.opt_text (CCOption.map Uuid.Actor.to_string owner)
            ]
          |> lwt_return_rc
        in
        lwt_return_rc (step stmt)
      ;;

      let find ?ctx ~typ _ =
        let (_ : (string * string) list option) = ctx in
        let _ = typ in
        failwith "TODO"
      ;;

      let find_roles ?ctx id =
        let open Lwt_result.Syntax in
        let open Sqlite3 in
        let (_ : (string * string) list option) = ctx in
        let id' = Uuid.Actor.to_string id in
        let stmt =
          prepare db {sql|SELECT roles FROM guardian_entities WHERE id = ?|sql}
        in
        let (_ : Rc.t) = bind_values stmt [ Data.TEXT id' ] in
        let* () = lwt_return_rc (step stmt) in
        match CCString.trim (column_text stmt 0) with
        | "" -> Lwt.return_ok Set.empty
        | coltext ->
          Yojson.Safe.from_string coltext |> Set.of_yojson |> Lwt.return
      ;;

      let find_owner ?ctx id =
        let open Lwt_result.Syntax in
        let open Sqlite3 in
        let (_ : (string * string) list option) = ctx in
        let id' = Uuid.Actor.to_string id in
        let stmt =
          prepare db {sql|SELECT parent FROM guardian_entities WHERE id = ?|sql}
        in
        let* () =
          let (_ : Rc.t) = bind_values stmt [ Data.TEXT id' ] in
          lwt_return_rc (step stmt)
        in
        let raw = column_text stmt 0 in
        match Uuid.Actor.of_string raw with
        | Some uuid -> Lwt.return_ok (Some uuid)
        | None -> Lwt.return_ok None
      ;;

      let save_rule ?ctx ((actor, action, target) : auth_rule) =
        let open Sqlite3 in
        let (_ : (string * string) list option) = ctx in
        let action' = Guardian.Action.to_string action in
        let stmt =
          match actor, target with
          | `Actor aid, `Target tid ->
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
                  [ TEXT (Uuid.Actor.to_string aid)
                  ; TEXT action'
                  ; TEXT (Uuid.Target.to_string tid)
                  ]
            in
            stmt
          | `Actor aid, `TargetEntity trole ->
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
                  [ TEXT (Uuid.Actor.to_string aid)
                  ; TEXT action'
                  ; TEXT (trole |> Guardian.to_actor |> A.show)
                  ]
            in
            stmt
          | `ActorEntity arole, `Target tid ->
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
                  [ TEXT (A.show arole)
                  ; TEXT action'
                  ; TEXT (Uuid.Target.to_string tid)
                  ]
            in
            stmt
          | `ActorEntity arole, `TargetEntity trole ->
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
                Data.
                  [ TEXT (A.show arole)
                  ; TEXT action'
                  ; TEXT (trole |> Guardian.to_actor |> A.show)
                  ]
            in
            stmt
        in
        lwt_return_rc (step stmt)
      ;;

      let delete_rule
        ?ctx
        ((actor, action, target) : Guardian.Authorizer.auth_rule)
        =
        let open Lwt_result.Syntax in
        let open Sqlite3 in
        let (_ : (string * string) list option) = ctx in
        match actor, target with
        | `Actor aid, `Target tid ->
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
                   [ TEXT (Uuid.Actor.to_string aid)
                   ; TEXT (Guardian.Action.to_string action)
                   ; TEXT (Uuid.Target.to_string tid)
                   ])
          in
          lwt_return_rc (step stmt)
        | `Actor aid, `TargetEntity trole ->
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
                   [ TEXT (Uuid.Actor.to_string aid)
                   ; TEXT (Guardian.Action.to_string action)
                   ; TEXT (trole |> Guardian.to_actor |> A.show)
                   ])
          in
          lwt_return_rc (step stmt)
        | `ActorEntity arole, `Target tid ->
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
                   [ TEXT (A.show arole)
                   ; TEXT (Guardian.Action.to_string action)
                   ; TEXT (Uuid.Target.to_string tid)
                   ])
          in
          lwt_return_rc (step stmt)
        | `ActorEntity arole, `TargetEntity trole ->
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
                   [ TEXT (A.show arole)
                   ; TEXT (Guardian.Action.to_string action)
                   ; TEXT (trole |> Guardian.to_actor |> A.show)
                   ])
          in
          lwt_return_rc (step stmt)
      ;;

      let find_rules ?ctx (spec : target_spec) =
        let open Lwt_result.Syntax in
        let open Sqlite3 in
        let (_ : (string * string) list option) = ctx in
        let* stmt =
          match spec with
          | `Target tid ->
            let stmt =
              {sql|
              SELECT act, actor_id, actor_role FROM guardian_rules WHERE target_id = ?
            |sql}
              |> prepare db
            in
            let* () =
              lwt_return_rc
                (bind_values stmt [ Data.TEXT (Uuid.Target.to_string tid) ])
            in
            Lwt.return_ok stmt
          | `TargetEntity role ->
            let stmt =
              {sql|
              SELECT act, actor_id, actor_role FROM guardian_rules WHERE target_role = ?
            |sql}
              |> prepare db
            in
            let () =
              Printf.printf
                "Searching for role: %s\n"
                (role |> Guardian.to_actor |> A.show)
            in
            let* () =
              lwt_return_rc
                (bind_values
                   stmt
                   [ Data.TEXT (role |> Guardian.to_actor |> A.show) ])
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
                  `Actor
                    (Uuid.Actor.of_string actor_id
                    |> CCOption.get_exn_or "Malformatted UUID!")
                | None, Some actor_role -> `ActorEntity (A.of_string actor_role)
                | _ -> raise (Invalid_argument "Invalid actor spec")
              in
              let x : Guardian.Authorizer.auth_rule =
                actor_spec, action, spec
              in
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
        let (_ : Rc.t) =
          bind_values stmt [ Data.TEXT (Uuid.Actor.to_string id) ]
        in
        let (_ : Rc.t), rv =
          fold stmt ~f:(fun _acc _row -> Some true) ~init:None
        in
        rv |> CCOption.get_or ~default:false |> Lwt.return_ok
      ;;

      let grant_roles ?ctx id roles =
        let open Lwt_result.Syntax in
        let open Sqlite3 in
        let (_ : (string * string) list option) = ctx in
        let id' = Uuid.Actor.to_string id in
        let* roles' =
          let* pre_roles = find_roles id in
          Set.union pre_roles roles
          |> Set.to_yojson
          |> Yojson.Safe.to_string
          |> Lwt.return_ok
        in
        let* stmt =
          let* mem = mem_authorizable ?ctx id in
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
        let open Lwt_result.Syntax in
        let open Sqlite3 in
        let (_ : (string * string) list option) = ctx in
        let id' = Uuid.Actor.to_string id in
        let* roles' =
          let* pre_roles = find_roles id in
          Set.diff pre_roles roles
          |> Set.to_yojson
          |> Yojson.Safe.to_string
          |> Lwt.return_ok
        in
        let* stmt =
          let* mem = mem_authorizable ?ctx id in
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
        let id' = Uuid.Actor.to_string id in
        let owner' = Uuid.Actor.to_string owner in
        let stmt =
          prepare
            db
            {sql|UPDATE guardian_entities SET parent = ? WHERE id = ?|sql}
        in
        let () = ignore (bind_values stmt Data.[ TEXT owner'; TEXT id' ]) in
        lwt_return_rc (step stmt)
      ;;
    end

    module Target = struct
      let find ?ctx ~typ _ =
        let (_ : (string * string) list option) = ctx in
        let _ = typ in
        failwith "TODO"
      ;;

      let find_roles ?ctx (id : Uuid.Target.t) =
        let open Lwt_result.Syntax in
        let open Sqlite3 in
        let (_ : (string * string) list option) = ctx in
        let id' = Uuid.Target.to_string id in
        let stmt =
          prepare db {sql|SELECT roles FROM guardian_targets WHERE id = ?|sql}
        in
        let (_ : Rc.t) = bind_values stmt [ Data.TEXT id' ] in
        let* () = lwt_return_rc (step stmt) in
        match CCString.trim (column_text stmt 0) with
        | "" -> Lwt.return_ok Guardian.TargetRoleSet.empty
        | coltext ->
          Yojson.Safe.from_string coltext
          |> Guardian.TargetRoleSet.of_yojson
          |> Lwt.return
      ;;

      let create ?ctx ~id ~owner roles : (unit, string) Lwt_result.t =
        let open Lwt_result.Syntax in
        let open Sqlite3 in
        let (_ : (string * string) list option) = ctx in
        let stmt =
          prepare
            db
            {sql|INSERT INTO guardian_targets (id, roles, parent) VALUES (?, ?, ?)|sql}
        in
        let roles' =
          Guardian.TargetRoleSet.to_yojson roles |> Yojson.Safe.to_string
        in
        let* () =
          bind_values
            stmt
            [ Data.TEXT (Uuid.Target.to_string id)
            ; Data.TEXT roles'
            ; Data.TEXT (Uuid.Actor.to_string owner)
            ]
          |> lwt_return_rc
        in
        lwt_return_rc (step stmt)
      ;;

      let mem ?ctx id =
        let open Sqlite3 in
        let (_ : (string * string) list option) = ctx in
        let stmt =
          prepare db {sql|SELECT * FROM guardian_entities WHERE id = ?|sql}
        in
        let (_ : Rc.t) =
          bind_values stmt [ Data.TEXT (Uuid.Target.to_string id) ]
        in
        let (_ : Rc.t), rv =
          fold stmt ~f:(fun _acc _row -> Some true) ~init:None
        in
        rv |> CCOption.get_or ~default:false |> Lwt.return_ok
      ;;

      let find_owner ?ctx id =
        let open Lwt_result.Syntax in
        let open Sqlite3 in
        let (_ : (string * string) list option) = ctx in
        let id' = Uuid.Target.to_string id in
        let stmt =
          prepare db {sql|SELECT parent FROM guardian_targets WHERE id = ?|sql}
        in
        let* () =
          let (_ : Rc.t) = bind_values stmt [ Data.TEXT id' ] in
          lwt_return_rc (step stmt)
        in
        let raw = column_text stmt 0 in
        match Uuid.Actor.of_string raw with
        | Some uuid -> Lwt.return_ok (Some uuid)
        | None -> Lwt.return_ok None
      ;;

      let save_owner ?ctx id ~owner =
        let open Sqlite3 in
        let (_ : (string * string) list option) = ctx in
        let id' = Uuid.Target.to_string id in
        let owner' = Uuid.Actor.to_string owner in
        let stmt =
          prepare
            db
            {sql|UPDATE guardian_targets SET parent = ? WHERE id = ?|sql}
        in
        let () = ignore (bind_values stmt Data.[ TEXT owner'; TEXT id' ]) in
        lwt_return_rc (step stmt)
      ;;
    end

    (** [find_migrations ()] returns a list of all migrations as a tuple with
        key, datetime and sql query **)
    let find_migrations () = Migrations.all

    (** [find_clean ()] returns a list of all migrations as a tuple with key and
        sql query **)
    let find_clean () =
      Migrations.all_tables
      |> CCList.map (fun m -> m, Format.asprintf "DELETE FROM %s" m)
    ;;

    (** [migrate ()] runs all migration on a specified context [?ctx] **)
    let migrate ?ctx () =
      let (_ : (string * string) list option) = ctx in
      ()
      |> find_migrations
      |> Lwt_list.iter_s (fun (key, date, sql) ->
           let open Sqlite3 in
           let open Lwt.Infix in
           Logs.debug (fun m -> m "Migration: Run '%s' from '%s'" key date);
           sql |> prepare db |> step |> lwt_return_rc >>= get_or_exn)
    ;;

    (** [clean ()] runs clean on a specified context [?ctx] **)
    let clean ?ctx () =
      let (_ : (string * string) list option) = ctx in
      ()
      |> find_clean
      |> Lwt_list.iter_s (fun (key, sql) ->
           let open Sqlite3 in
           let open Lwt.Infix in
           Logs.debug (fun m -> m "Clean: Run '%s'" key);
           sql |> prepare db |> step |> lwt_return_rc >>= get_or_exn)
    ;;
  end

  include Guardian.Make_persistence (Backend)
end
