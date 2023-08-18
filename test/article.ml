module Make (Backend : Guard.PersistenceSig) = struct
  open Guard
  module User = User.MakeActor (Backend)

  (* pretend that all these fields aren't publically visible *)
  type t =
    { mutable title : string
    ; mutable content : string
    ; mutable author : User.t
    ; id : Uuid.Target.t
    }
  [@@deriving show]

  let make ?id title content author =
    let id = CCOption.get_or ~default:(Uuid.Target.create ()) id in
    { id; title; content; author }
  ;;

  let to_authorizable ?ctx { id; author; _ } =
    let%lwt auth = Backend.Target.decorate ?ctx (Target.create `Article) id in
    let%lwt () =
      Guard.ActorRole.create ~target_uuid:id (snd author) `Author
      |> Backend.ActorRole.upsert ?ctx
    in
    Lwt.return auth
  ;;

  let update_title ?ctx (actor : Actor.t) t new_title =
    let open Lwt_result.Syntax in
    let f new_title =
      let () = t.title <- new_title in
      Lwt.return_ok t
    in
    let* wrapped =
      Backend.wrap_function
        ?ctx
        CCFun.id
        ValidationSet.(One (Permission.Update, TargetEntity.Id t.id))
        f
    in
    wrapped actor new_title
  ;;

  let update_author
      ?ctx
      (actor : Actor.t)
      ({ id; _ } as article)
      old_author
      new_author
    =
    let open Lwt_result.Syntax in
    let fcn (old_author, new_author) =
      let create_role author =
        Guard.ActorRole.create ~target_uuid:id (snd author) `Author
      in
      let%lwt () = create_role old_author |> Backend.ActorRole.delete ?ctx in
      let%lwt () = create_role new_author |> Backend.ActorRole.upsert ?ctx in
      Lwt.return_ok article
    in
    let* wrapped =
      Backend.wrap_function
        ?ctx
        CCFun.id
        ValidationSet.(One (Permission.Manage, TargetEntity.Id id))
        fcn
    in
    wrapped actor (old_author, new_author)
  ;;
end
