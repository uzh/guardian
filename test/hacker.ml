module Make (Backend : Guard.PersistenceSig) = struct
  open Guard

  type t = string * Uuid.Actor.t

  let make s : t = s, Uuid.Actor.create ()

  let to_authorizable ?ctx =
    Backend.Actor.decorate ?ctx (fun (t : t) : Actor.t ->
        Actor.create `Hacker (snd t))
  ;;
end
