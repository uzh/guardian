module Make (Backend : Guard.PersistenceSig) = struct
  open Guard
  module User = User.MakeActor (Backend)

  (* pretend that all these fields aren't publically visible *)
  type t =
    { mutable title : string
    ; mutable content : string
    ; mutable author : User.t
    ; uuid : Uuid.Target.t
    }
  [@@deriving show]

  let make ?id title content author =
    let uuid = CCOption.get_or ~default:(Uuid.Target.create ()) id in
    { uuid; title; content; author }
  ;;

  let to_authorizable ?ctx =
    Backend.Target.decorate ?ctx (fun t ->
      Target.make ~owner:(snd t.author) `Article t.uuid)
  ;;

  let update_title ?ctx (actor : [ `User ] Actor.t) t new_title =
    let open Lwt_result.Syntax in
    let f new_title =
      let () = t.title <- new_title in
      Lwt.return_ok t
    in
    let* wrapped =
      Backend.wrap_function
        ?ctx
        CCFun.id
        ValidationSet.(One (Action.Update, TargetSpec.Id (`Article, t.uuid)))
        f
    in
    wrapped actor new_title
  ;;

  let update_title_by_role ?ctx (actor : [ `User ] Actor.t) t new_title =
    let open Lwt_result.Syntax in
    let f new_title =
      let () = t.title <- new_title in
      Lwt.return_ok t
    in
    let* wrapped =
      Backend.wrap_function
        ?ctx
        CCFun.id
        ValidationSet.(SpecificRole (`Editor t.uuid))
        f
    in
    wrapped actor new_title
  ;;

  let update_author ?ctx (actor : [ `User ] Actor.t) t new_author =
    let open Lwt_result.Syntax in
    let f new_author =
      let () = t.author <- new_author in
      let* ent = to_authorizable ?ctx t in
      let* () =
        Backend.Target.save_owner ?ctx ~owner:(snd new_author) (ent |> Target.id)
      in
      Lwt.return_ok t
    in
    let* wrapped =
      Backend.wrap_function
        ?ctx
        CCFun.id
        ValidationSet.(One (Action.Manage, TargetSpec.Id (`Article, t.uuid)))
        f
    in
    wrapped actor new_author
  ;;
end
