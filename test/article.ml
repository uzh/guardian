(** pretend that all these fields aren't publically visible *)
type t =
  { mutable title: string
  ; mutable content: string
  ; mutable author: User.t
  ; uuid: Uuidm.t
  } [@@deriving make]

type kind = [ `Article ]

let to_entity t =
  let open Ocaml_authorize in
  let owner =
    match User.to_entity t.author with
    | Ok ent -> Some {ent with typ = ()}
    | Error _ -> None
  in
  let roles =
    Ocaml_authorize.Role_set.empty
    |> Ocaml_authorize.Role_set.add "Article"
  in
  Entity.make
    ~roles
    ?owner
    ~typ:`Article
    (snd t.author)

let to_entity =
  Ocauth_store.decorate_to_entity to_entity

let update_title (actor: [`User | `Article] Ocaml_authorize.Entity.t) t new_title =
  let can =
    Ocaml_authorize.Authorizer.make_checker
      [ ("Admin", [`Create; `Read; `Update; `Delete]) ]
  in
  let ( let* ) = Result.bind in
  let* ent = to_entity t in
  if can actor `Update ent
  then let _ = t.title <- new_title in Ok t
  else Error "Insufficient access"
