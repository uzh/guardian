module Make (P : Guard.Persistence_s) = struct
  type t = string * Guardian.Uuid.Actor.t [@@deriving show]
  type kind = [ `User ]

  let make s : t = s, Guardian.Uuid.Actor.create ()

  let to_authorizable (t : t) =
    let open Guard in
    Authorizable.make ~roles:(Role_set.singleton `User) ~typ:`User (snd t)
  ;;

  let to_authorizable ?ctx = P.decorate_to_authorizable ?ctx to_authorizable
end
