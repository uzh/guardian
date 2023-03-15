open CCFun.Infix
open Guard

type user = string * Uuid.Actor.t [@@deriving show]

module MakeActor (Backend : PersistenceSig) = struct
  type t = user [@@deriving show]

  let make s : t = s, Uuid.Actor.create ()

  let to_authorizable ?ctx =
    Backend.Actor.decorate ?ctx (fun (t : t) : [> `User ] Actor.t ->
      Actor.make (RoleSet.singleton `User) `User (snd t))
  ;;

  let update_name ?ctx (actor : [ `User | `Admin ] Actor.t) t new_name =
    let open Lwt_result.Syntax in
    let f new_name = Lwt.return_ok (new_name, snd t) in
    let* wrapped =
      Backend.wrap_function
        ?ctx
        CCFun.id
        EffectSet.(One (Action.Update, TargetSpec.Id (`User, snd t)))
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
      Target.make `User (of_actor t))
  ;;
end
