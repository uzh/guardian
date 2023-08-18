module Make (Backend : Guard.PersistenceSig) = struct
  open Guard
  module User = User.MakeActor (Backend)

  (* pretend that all these fields aren't publically visible *)
  type t =
    { id : Uuid.Target.t
    ; mutable note : string
    ; author : User.t
    }
  [@@deriving show]

  let make ?(id = Uuid.Target.create ()) author note = { id; note; author }

  let to_authorizable ?ctx { id; author; _ } =
    let%lwt auth = Backend.Target.decorate ?ctx (Target.create `Note) id in
    let%lwt () =
      Guard.ActorRole.create ~target_uuid:id (snd author) `Author
      |> Backend.ActorRole.upsert ?ctx
    in
    Lwt.return auth
  ;;

  let update_note ?ctx (actor : Actor.t) t new_note =
    let open Lwt_result.Syntax in
    let f new_note =
      let () = t.note <- new_note in
      Lwt.return_ok t
    in
    let* () =
      Backend.validate
        ?ctx
        CCFun.id
        ValidationSet.(One (Permission.Update, TargetEntity.Id t.id))
        actor
    in
    f new_note
  ;;
end
