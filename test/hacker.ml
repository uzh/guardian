module Make(P : Ocaml_authorize.Persistence.S) = struct
  type t = string * Uuidm.t

  let make s : t = s, Uuidm.create `V4

  let to_authorizable (t : t): [ `Hacker ] Ocaml_authorize.Authorizable.t =
    Ocaml_authorize.Authorizable.make
      ~roles:(Ocaml_authorize.Role_set.of_list ["Hacker"])
      ~typ:`Hacker
      (snd t)

  let to_authorizable = P.decorate_to_authorizable to_authorizable
end
