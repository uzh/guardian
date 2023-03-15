module Make (Backend : Guard.PersistenceSig) = struct
  open Guard

  type t = string * Uuid.Actor.t

  let make s : t = s, Uuid.Actor.create ()

  let to_authorizable ?ctx =
    Backend.Actor.decorate ?ctx (fun (t : t) : [ `Hacker ] Actor.t ->
      Actor.make (RoleSet.singleton `Hacker) `Hacker (snd t))
  ;;
end
