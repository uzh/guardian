(* open Lwt.Infix

module Db =
  (val Caqti_lwt.connect
      (Uri.of_string "127.0.0.1:3306")
      >>= Caqti_lwt.or_fail
        |> Lwt_main.run)

include Ocaml_authorize.Persistence.Make(struct
  let get_roles id =
    Lwt.
    let caqti =
      Caqti_request.find
        Caqti_type.int
        Caqti_type.string
        "SELECT roles FROM entities WHERE id = ?"
    in
    match Db.find caqti id with
    | Ok s -> Ocaml_authorize.Role_set.of_yojson(Yojson.Safe.from_string s)
    | Error err -> Caqti_error.show err

  let get_perms _target_spec =
    Error "unimplemented"
  let put_perm _auth_rule =
    Error "unimplemented"
  let delete_perm _auth_rule =
    Error "unimplemented"
  let grant_roles _uuid _roles =
    Error "unimplemented"
  let create_entity ~id:_ ?owner:_ _roles =
    Error "unimplemented"
  let mem_entity _id =
    let _ = raise(Invalid_argument "unimplemented") in
    true
  let get_owner _id =
    Error "unimplemented"
  let set_owner _id ~owner:_ =
    Error "unimplemented"
end) *)