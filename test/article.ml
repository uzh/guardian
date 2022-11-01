module Make (P : Guard.Persistence_s) = struct
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
    let open Guard in
    let roles = Role_set.singleton `Article in
    Authorizable.make ~roles ~typ:`Article ~owner:(snd t.author) t.uuid
  ;;

  let to_authorizable ?ctx = P.decorate_to_authorizable ?ctx to_authorizable

  let update_title
    ?ctx
    (actor : [ `User | `Article ] Guard.Authorizable.t)
    t
    new_title
    =
    let open Lwt_result.Syntax in
    let f new_title =
      let () = t.title <- new_title in
      Lwt.return_ok t
    in
    let* wrapped =
      P.wrap_function ?ctx ~error:CCFun.id ~effects:[ `Update, `One t.uuid ] f
    in
    wrapped ~actor new_title
  ;;

  let update_author ?ctx (actor : [ `User ] Guard.Authorizable.t) t new_author =
    let open Lwt_result.Syntax in
    let f new_author =
      let () = t.author <- new_author in
      let* ent = to_authorizable ?ctx t in
      let* () = P.save_owner ?ctx ent.uuid ~owner:(snd new_author) in
      Lwt.return_ok t
    in
    let* wrapped =
      P.wrap_function ?ctx ~error:CCFun.id ~effects:[ `Manage, `One t.uuid ] f
    in
    wrapped ~actor new_author
  ;;
end
