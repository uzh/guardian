type t = string * Uuidm.t

type kind = [ `User ]

let make s : t = s, Uuidm.create `V4

let to_entity (t: t) =
  let open Ocaml_authorize in
  Entity.make
    ~roles:(Role_set.of_list ("User" :: User_store.get_roles t))
    ~typ:`User
    (snd t)
let to_entity =
  Ocauth_store.decorate_to_entity to_entity

(* let can:
   [ `User] Ocaml_authorize.Entity.t ->
   Ocaml_authorize.Action.t ->
   [ `User] Ocaml_authorize.Entity.t ->
   bool
   =
   Ocaml_authorize.Authorizer.make_checker
    [ ("Admin", [`Create; `Read; `Update; `Delete]) ] *)