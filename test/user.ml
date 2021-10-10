type t = string * Uuidm.t

let make s = s, Uuidm.create `V4

let to_entity (t: t) =
  let open Ocaml_authorize in
  Entity.make
    ~roles:(Role_set.of_list ("User" :: User_store.get_roles t))
    (snd t)

let can =
  Ocaml_authorize.Authorizer.make_checker
    [ ("Admin", [`Create; `Read; `Update; `Delete]) ]