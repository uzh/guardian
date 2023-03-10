module Make (P : Guard.Persistence_s) = struct
  module User = User.MakeActor (P)

  (* pretend that all these fields aren't publically visible *)
  type t =
    { mutable title : string
    ; mutable content : string
    ; mutable author : User.t
    ; uuid : Guardian.Uuid.Target.t
    }
  [@@deriving show]

  let make ?id title content author =
    let uuid = CCOption.get_or ~default:(Guardian.Uuid.Target.create ()) id in
    { uuid; title; content; author }
  ;;

  let to_authorizable ?ctx =
    let open Guard in
    P.Target.decorate ?ctx (fun t ->
      Target.make ~owner:(snd t.author) `Article t.uuid)
  ;;

  let update_title ?ctx (actor : [ `User ] Guard.Actor.t) t new_title =
    let open Lwt_result.Syntax in
    let open Guard in
    let f new_title =
      let () = t.title <- new_title in
      Lwt.return_ok t
    in
    let* wrapped =
      P.wrap_function
        ?ctx
        CCFun.id
        EffectSet.(
          One (Guardian.Action.Update, TargetSpec.Id (`Article, t.uuid)))
        f
    in
    wrapped actor new_title
  ;;

  let update_author ?ctx (actor : [ `User ] Guard.Actor.t) t new_author =
    let open Lwt_result.Syntax in
    let open Guard in
    let f new_author =
      let () = t.author <- new_author in
      let* ent = to_authorizable ?ctx t in
      let* () =
        P.Target.save_owner ?ctx ~owner:(snd new_author) ent.Target.uuid
      in
      Lwt.return_ok t
    in
    let* wrapped =
      P.wrap_function
        ?ctx
        CCFun.id
        EffectSet.(
          One (Guardian.Action.Manage, TargetSpec.Id (`Article, t.uuid)))
        f
    in
    wrapped actor new_author
  ;;
end
