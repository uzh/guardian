module Make (Backend : Guard.PersistenceSig) = struct
  open Guard
  module User = User.MakeActor (Backend)
  module Article = Article.Make (Backend)

  let (_ : (unit, string) result) =
    Backend.Dependency.register
      ~parent:`Article
      `Post
      (fun ?ctx:_ (action, spec) ->
      match[@warning "-4"] spec with
      | TargetSpec.Entity `Post | TargetSpec.Id (`Post, _) ->
        Lwt.return_ok (Some (action, TargetSpec.Entity `Article))
      | _ -> Lwt.return_error "Invalid entity provided")
  ;;

  (* pretend that all these fields aren't publically visible *)
  type t =
    { id : Uuid.Target.t
    ; mutable comment : string
    ; author : User.t
    ; article : Article.t
    }
  [@@deriving show]

  let make ?(id = Uuid.Target.create ()) author article comment =
    { id; comment; article; author }
  ;;

  let to_authorizable ?ctx =
    Backend.Target.decorate ?ctx (fun t ->
      Target.make ~owner:(snd t.author) `Post t.id)
  ;;

  let update_post ?ctx (actor : [ `User ] Actor.t) t new_comment =
    let open Lwt_result.Syntax in
    let f new_comment =
      let () = t.comment <- new_comment in
      Lwt.return_ok t
    in
    let* () =
      Backend.validate
        ?ctx
        CCFun.id
        ValidationSet.(One (Action.Update, TargetSpec.Id (`Post, t.id)))
        actor
    in
    f new_comment
  ;;
end
