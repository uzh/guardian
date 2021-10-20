type t = string * Uuidm.t

let make s : t = s, Uuidm.create `V4

let to_entity t =
  Ocaml_authorize.Entity.make
    ~roles:(Ocaml_authorize.Role_set.of_list ["Hacker"])
    ~typ:`Hacker
    (snd t)