type user = string * Guardian.Uuid.Actor.t [@@deriving show]
type user_kind = [ `User ]

module MakeActor (P : Guard.Persistence_s) = struct
  type t = user [@@deriving show]
  type kind = user_kind

  let make s : t = s, Guardian.Uuid.Actor.create ()

  let to_authorizable ?ctx =
    let open Guard in
    P.Actor.decorate ?ctx (fun (t : t) : [> `User ] Authorizable.t ->
      Authorizable.make (ActorRoleSet.singleton `User) `User (snd t))
  ;;

  let update_name
    ?ctx
    (actor : [ `User | `Admin ] Guard.Authorizable.t)
    t
    new_name
    =
    let open Lwt_result.Syntax in
    let f new_name = Lwt.return_ok (new_name, snd t) in
    let* wrapped =
      P.wrap_function ?ctx CCFun.id [ `Update, `Target (snd t) ] f
    in
    wrapped actor new_name
  ;;
end

module MakeTarget (P : Guard.Persistence_s) = struct
  type t = user [@@deriving show]
  type kind = user_kind

  let to_authorizable ?ctx =
    let open Guard in
    P.Target.decorate ?ctx (fun t ->
      let of_actor =
        CCFun.(Uuid.(snd %> Actor.to_string %> Target.of_string_exn))
      in
      AuthorizableTarget.make (TargetRoleSet.singleton `User) `User (of_actor t))
  ;;
end
