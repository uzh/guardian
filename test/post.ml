module Make (P : Guard.Persistence_s) = struct
  module User = User.MakeActor (P)
  module Article = Article.Make (P)

  let (_ : (unit, string) result) =
    P.Dependency.register `Post `Article (fun ?ctx:_ (action, spec) ->
      match spec with
      | `TargetEntity `Post | `Target (`Post, _) ->
        Lwt.return_ok (Some (action, `TargetEntity `Article))
      | _ -> Lwt.return_error "Invalid entity provided")
  ;;

  (* pretend that all these fields aren't publically visible *)
  type t =
    { uuid : Guardian.Uuid.Target.t
    ; mutable comment : string
    ; author : User.t
    ; article : Article.t
    }
  [@@deriving show]

  let make ?id author article comment =
    let uuid = CCOption.get_or ~default:(Guard.Uuid.Target.create ()) id in
    { uuid; comment; article; author }
  ;;

  let to_authorizable ?ctx =
    let open Guard in
    P.Target.decorate ?ctx (fun t ->
      AuthorizableTarget.make ~owner:(snd t.author) `Post t.uuid)
  ;;

  let update_post ?ctx (actor : [ `User ] Guard.Authorizable.t) t new_comment =
    let open Lwt_result.Syntax in
    let f new_comment =
      let () = t.comment <- new_comment in
      Lwt.return_ok t
    in
    let* () =
      P.validate_effects
        ?ctx
        Guard.(AuthenticationSet.One (Action.Update, `Target (`Post, t.uuid)))
        actor
    in
    f new_comment
  ;;
end
