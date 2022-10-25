module Make (P : Ocauth.Persistence_s) = struct
  type t = string * Uuidm.t

  let make s : t = s, Uuidm.v `V4

  let to_authorizable (t : t) : [ `Hacker ] Ocauth.Authorizable.t =
    Ocauth.Authorizable.make
      ~roles:(Ocauth.Role_set.of_list [ `Hacker ])
      ~typ:`Hacker
      (snd t)
  ;;

  let to_authorizable ?ctx = P.decorate_to_authorizable ?ctx to_authorizable
end
