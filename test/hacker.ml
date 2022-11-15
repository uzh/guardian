module Make (P : Guard.Persistence_s) = struct
  type t = string * Guardian.Uuid.Actor.t

  let make s : t = s, Guardian.Uuid.Actor.create ()

  let to_authorizable ?ctx =
    P.Actor.decorate ?ctx (fun (t : t) : [ `Hacker ] Guard.Authorizable.t ->
      Guard.Authorizable.make
        ~roles:(Guard.ActorRoleSet.of_list [ `Hacker ])
        ~typ:`Hacker
        (snd t))
  ;;
end
