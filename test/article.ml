(** pretend that all these fields aren't publically visible *)
type t =
  { mutable title: string
  ; mutable content: string
  ; mutable author: User.t
  } [@@deriving make]

type kind = [ `Article ]

let to_entity t =
  let open Ocaml_authorize in
  let owner =
    let acc = User.to_entity t.author in
    {acc with typ = Some ()}
  in
  Entity.make
    ~roles:Role_set.(add "Article" empty)
    ~owner
    (snd t.author)

type _ permitted_actor =
  | Article : t -> t permitted_actor
  | User : User.t -> User.t permitted_actor

let update_title (actor: [`User | `Article] Ocaml_authorize.Entity.t) t new_title =
  let can =
    Ocaml_authorize.Authorizer.make_checker
      [ ("Admin", [`Create; `Read; `Update; `Delete]) ]
  in
  if can actor `Update (to_entity t)
  then let _ = t.title <- new_title in Ok t
  else Error "Insufficient access"
