open CCFun.Infix
open Guard

type user = string * Uuid.Actor.t [@@deriving show]

module MakeActor (Backend : PersistenceSig) = struct
  type t = user [@@deriving show]

  let make s : t = s, Uuid.Actor.create ()

  let insert_roles ?ctx actor =
    let target_uuid =
      actor.Actor.uuid |> Uuid.Actor.to_string |> Uuid.Target.of_string_exn
    in
    let%lwt () =
      ActorRole.create actor.Actor.uuid `Reader |> Backend.ActorRole.upsert ?ctx
    in
    ActorPermission.create_for_id actor.Actor.uuid Permission.Update target_uuid
    |> Backend.ActorPermission.insert ?ctx
  ;;

  let to_authorizable ?ctx actor : (Backend.actor, string) result Lwt.t =
    let open Lwt_result.Syntax in
    let* actor_ent =
      Backend.Actor.decorate
        ?ctx
        (fun (t : t) : Actor.t -> Actor.create `User (snd t))
        actor
    in
    (* A user is always also a target (e.g. for self-update permissions) *)
    let target_uuid =
      actor_ent.Actor.uuid |> Uuid.Actor.to_string |> Uuid.Target.of_string_exn
    in
    let* () = Backend.Target.insert ?ctx (Target.create `User target_uuid) in
    Lwt.return_ok actor_ent
  ;;

  let update_name ?ctx (actor : Actor.t) t new_name =
    let open Lwt_result.Syntax in
    let f new_name = Lwt.return_ok (new_name, snd t) in
    let* wrapped =
      Backend.wrap_function
        ?ctx
        CCFun.id
        ValidationSet.(one_of_tuple (Permission.Update, `User, Some (snd t)))
        f
    in
    wrapped actor new_name
  ;;
end

module MakeTarget (Backend : PersistenceSig) = struct
  type t = user [@@deriving show]

  let to_authorizable ?ctx =
    Backend.Target.decorate ?ctx (fun t ->
      let of_actor = Uuid.(snd %> Actor.to_string %> Target.of_string_exn) in
      Target.create `User (of_actor t))
  ;;
end
