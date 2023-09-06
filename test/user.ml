open CCFun.Infix
open Guard

type user = string * Uuid.Actor.t [@@deriving show]

module MakeActor (Backend : PersistenceSig) = struct
  type t = user [@@deriving show]

  let make s : t = s, Uuid.Actor.create ()

  let insert_roles ?ctx actor =
    let%lwt () =
      ActorRole.create actor.Actor.uuid `Reader |> Backend.ActorRole.upsert ?ctx
    in
    ActorPermission.create_for_id
      actor.Actor.uuid
      Permission.Update
      (actor.Actor.uuid |> Uuid.(Actor.to_string %> Target.of_string_exn))
    |> Backend.ActorPermission.insert ?ctx
  ;;

  let to_authorizable ?ctx actor : (Backend.actor, string) result Lwt.t =
    Backend.Actor.decorate
      ?ctx
      (fun (t : t) : Actor.t -> Actor.create `User (snd t))
      actor
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
