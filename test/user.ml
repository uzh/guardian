module Make(P : Ocaml_authorize.Persistence.S) = struct
  type t = (string * Uuidm.t) [@@deriving show]

  type kind = [ `User ]

  let make s : t = s, Uuidm.create `V4

  let to_entity (t: t) =
    let open Ocaml_authorize in
    Entity.make
      ~roles:(Role_set.singleton "User")
      ~typ:`User
      (snd t)
  let to_entity =
    P.decorate_to_entity to_entity

  (* let can:
    [ `User] Ocaml_authorize.Entity.t ->
    Ocaml_authorize.Action.t ->
    [ `User] Ocaml_authorize.Entity.t ->
    bool
    =
    Ocaml_authorize.Authorizer.make_checker
      [ ("Admin", [`Create; `Read; `Update; `Delete]) ] *)
end
