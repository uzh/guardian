(** pretend that all these fields aren't publically visible *)
type t =
  { mutable title: string
  ; mutable content: string
  ; mutable author: User.t
  } [@@deriving make]

let to_entity t =
  let open Ocaml_authorize in
  Entity.make
    ~roles:Role_set.(add "Article" empty)
    ~owner:(User.to_entity t.author)
    (snd t.author)

let can =
  Ocaml_authorize.Authorizer.make_checker
    [ ("Admin", [`Create; `Read; `Update; `Delete]) ]

let update_title actor (t: t) new_title =
  if can actor `Update (to_entity t)
  then let _ = t.title <- new_title in Ok t
  else Error "Insufficient access"