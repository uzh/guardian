type t = string

let to_entity t =
  let open Ocaml_authorize in
  Entity.make
    ~roles:(Role_set.of_list ("User" :: User_store.get_roles t))
    ~repr:t
    ()

let can =
  Ocaml_authorize.Authorizer.make_checker
    [ ("Admin", [`Create; `Read; `Update; `Delete]) ]