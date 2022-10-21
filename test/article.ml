module Make (P : Ocauth.Persistence_s) = struct
  module User = User.Make (P)

  (* pretend that all these fields aren't publically visible *)
  type t =
    { mutable title : string
    ; mutable content : string
    ; mutable author : User.t
    ; uuid : Uuidm.t
    }
  [@@deriving make, show]

  type kind = [ `Article ]

  let to_authorizable t =
    let open Ocauth in
    let roles = Ocauth.Role_set.singleton `Article in
    Authorizable.make ~roles ~typ:`Article ~owner:(snd t.author) t.uuid
  ;;

  let to_authorizable = P.decorate_to_authorizable to_authorizable

  let update_title
    (actor : [ `User | `Article ] Ocauth.Authorizable.t)
    t
    new_title
    =
    let f new_title =
      let _ = t.title <- new_title in
      Lwt.return_ok t
    in
    let ( let* ) = Lwt_result.bind in
    let* wrapped =
      P.wrap_function ~error:(fun x -> x) ~effects:[ `Update, `One t.uuid ] f
    in
    wrapped ~actor new_title
  ;;

  let update_author (actor : [ `User ] Ocauth.Authorizable.t) t new_author =
    let ( let* ) = Lwt_result.bind in
    let f new_author =
      let _ = t.author <- new_author in
      let* ent = to_authorizable t in
      let* () = P.set_owner ent.uuid ~owner:(snd new_author) in
      Lwt.return_ok t
    in
    let* wrapped =
      P.wrap_function ~error:(fun x -> x) ~effects:[ `Manage, `One t.uuid ] f
    in
    wrapped ~actor new_author
  ;;
end
