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

  let to_authorizable ?ctx =
    Backend.Target.decorate ?ctx (fun t ->
      Target.make ~owner:(snd t.author) `Note t.id)
  ;;

  let update_note ?ctx (actor : [ `User ] Actor.t) t new_note =
    let open Lwt_result.Syntax in
    let f new_note =
      let () = t.note <- new_note in
      Lwt.return_ok t
    in
    let* () =
      Backend.validate
        ?ctx
        CCFun.id
        ValidationSet.(One (Action.Update, TargetSpec.Id (`Note, t.id)))
        actor
    in
    f new_note
  ;;
end
