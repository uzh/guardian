module Uuidm = struct
  include Uuidm

  let to_yojson t =
    `String (to_string t)

  let of_yojson = function
    | `String s ->
      of_string s
      |> begin function
        | Some x -> Ok x
        | None -> Error("Invalid UUID: " ^ s)
      end
    | _ -> raise (Invalid_argument "")
end

module Make(R : Role.S) = struct
  module Action = Action

  module Role_set : Role_set.S = Role_set.Make(R)

  module Authorizable = Authorizable.Make(Role_set)

  module Authorizer = Authorizer.Make

  module Make_persistence = Persistence.Make
    (Authorizable)
    (R)
    (Role_set)
    (Authorizer)
end
