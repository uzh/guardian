open CCFun.Infix

type user = string * Guardian.Uuid.Actor.t [@@deriving show]

module MakeActor (P : Guard.Persistence_s) = struct
  type t = user [@@deriving show]

  let make s : t = s, Guardian.Uuid.Actor.create ()

  let to_authorizable ?ctx =
    let open Guard in
    P.Actor.decorate ?ctx (fun (t : t) : [> `User ] Actor.t ->
      Actor.make (RoleSet.singleton `User) `User (snd t))
  ;;

  let update_name ?ctx (actor : [ `User | `Admin ] Guard.Actor.t) t new_name =
    let open Lwt_result.Syntax in
    let open Guard in
    let f new_name = Lwt.return_ok (new_name, snd t) in
    let* wrapped =
      P.wrap_function
        ?ctx
        CCFun.id
        EffectSet.(One (Guardian.Action.Update, `Target (snd t)))
        f
    in
    wrapped actor new_name
  ;;
end

module MakeTarget (P : Guard.Persistence_s) = struct
  type t = user [@@deriving show]

  let to_authorizable ?ctx =
    let open Guard in
    P.Target.decorate ?ctx (fun t ->
      let of_actor =
        Guardian.Uuid.(snd %> Actor.to_string %> Target.of_string_exn)
      in
      Target.make `User (of_actor t))
  ;;
end
