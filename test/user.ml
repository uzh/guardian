module Make (P : Guard.Persistence_s) = struct
  type t = string * Uuidm.t [@@deriving show]
  type kind = [ `User ]

  let make s : t = s, Uuidm.v `V4

  let to_authorizable (t : t) =
    let open Guard in
    Authorizable.make ~roles:(Role_set.singleton `User) ~typ:`User (snd t)
  ;;

  let to_authorizable ?ctx = P.decorate_to_authorizable ?ctx to_authorizable
end
