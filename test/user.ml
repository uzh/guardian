module Make(P : Ocaml_authorize.Persistence.S) = struct
  type t = (string * Uuidm.t) [@@deriving show]

  type kind = [ `User ]

  let make s : t = s, Uuidm.create `V4

  let to_authorizable (t: t) =
    let open Ocaml_authorize in
    Authorizable.make
      ~roles:(Role_set.singleton "User")
      ~typ:`User
      (snd t)
  let to_authorizable =
    P.decorate_to_authorizable to_authorizable
end
