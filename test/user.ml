type user = string * Guardian.Uuid.Actor.t [@@deriving show]
type user_kind = [ `User ]

module MakeActor (P : Guard.Persistence_s) = struct
  type t = user [@@deriving show]
  type kind = user_kind

  let make s : t = s, Guardian.Uuid.Actor.create ()

  let to_authorizable ?ctx =
    let open Guard in
    P.Actor.decorate_to_authorizable ?ctx (fun (t : t) ->
      Authorizable.make ~roles:(ActorRoleSet.singleton `User) ~typ:`User (snd t))
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
      P.wrap_function
        ?ctx
        ~error:CCFun.id
        ~effects:[ `Update, `Target (snd t) ]
        f
    in
    wrapped ~actor new_name
  ;;
end

module MakeTarget (P : Guard.Persistence_s) = struct
  type t = user [@@deriving show]
  type kind = user_kind

  let to_authorizable ?ctx =
    let open Guard in
    P.Target.decorate
      ?ctx
      ~singleton:(TargetRoleSet.singleton `User)
      ~typ:`User
      (fun t ->
      AuthorizableTarget.make
        ~typ:`User
        ~owner:(snd t)
        (snd t |> Uuid.target_of_actor))
  ;;
end
