module Make (Backend : Guard.PersistenceSig) = struct
  type t = string * Guardian.Uuid.Actor.t

  let make s : t = s, Guardian.Uuid.Actor.create ()

  let to_authorizable ?ctx =
    Backend.Actor.decorate ?ctx (fun (t : t) : [ `Hacker ] Guard.Actor.t ->
      Guard.Actor.make (Guard.RoleSet.of_list [ `Hacker ]) `Hacker (snd t))
  ;;
end
