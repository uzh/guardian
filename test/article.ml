module Make(P : Ocaml_authorize.Persistence.S) = struct
  module User = User.Make(P)
  (** pretend that all these fields aren't publically visible *)
  type t =
    { mutable title: string
    ; mutable content: string
    ; mutable author: User.t
    ; uuid: Uuidm.t
    } [@@deriving make,show]

  type kind = [ `Article ]

  let to_entity t =
    let open Ocaml_authorize in
    let roles =
      Ocaml_authorize.Role_set.empty
      |> Ocaml_authorize.Role_set.add "Article"
    in
    Entity.make
      ~roles
      ~typ:`Article
      t.uuid

  (** This pattern may be instructive. *)
  let to_entity t =
    let%lwt ent = (P.decorate_to_entity to_entity) t in
    let%lwt owner =
      match%lwt User.to_entity t.author with
      | Ok own -> Lwt.return_some {own with typ = ()}
      | Error _ -> Lwt.return_none
    in
    match ent with
    | Ok ent' -> Lwt.return_ok {ent' with owner = owner}
    | err -> Lwt.return err

  let update_title (actor: [`User | `Article] Ocaml_authorize.Entity.t) t new_title =
    let ( let* ) = Lwt_result.bind in
    let* ent = to_entity t in
    let* can = P.get_checker ent in
    if can actor `Update
    then let _ = t.title <- new_title in Lwt.return_ok t
    else Lwt.return_error "Insufficient access"
  end
