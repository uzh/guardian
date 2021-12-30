module Make(P : Ocauth.Persistence_s) = struct
  module User = User.Make(P)
  (** pretend that all these fields aren't publically visible *)
  type t =
    { mutable title: string
    ; mutable content: string
    ; mutable author: User.t
    ; uuid: Uuidm.t
    } [@@deriving make,show]

  type kind = [ `Article ]

  let to_authorizable t =
    let open Ocauth in
    let roles =
      Ocauth.Role_set.empty
      |> Ocauth.Role_set.add `Article
    in
    Authorizable.make
      ~roles
      ~typ:`Article
      t.uuid

  (** This pattern may be instructive. *)
  let to_authorizable t =
    let%lwt ent = (P.decorate_to_authorizable to_authorizable) t in
    let%lwt owner =
      match%lwt User.to_authorizable t.author with
      | Ok own -> Lwt.return_some {own with typ = ()}
      | Error _ -> Lwt.return_none
    in
    match ent with
    | Ok ent' -> Lwt.return_ok {ent' with owner = owner}
    | err -> Lwt.return err

  let update_title (actor: [`User | `Article] Ocauth.Authorizable.t) t new_title =
    let ( let* ) = Lwt_result.bind in
    let* ent = to_authorizable t in
    let* can = P.get_checker ent in
    if can actor `Update
    then let _ = t.title <- new_title in Lwt.return_ok t
    else Lwt.return_error "Insufficient access"

  let update_author (actor: [`User] Ocauth.Authorizable.t) t new_author =
    let ( let* ) = Lwt_result.bind in
    let* ent = to_authorizable t in
    let* can = P.get_checker ent in
    if can actor `Update
    then
      let _ = t.author <- new_author in
      let* () = P.set_owner ent.uuid ~owner:(snd new_author) in
      Lwt.return_ok t
    else
      Lwt.return_error "Insufficient access"
end
