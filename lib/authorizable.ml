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

type 'a t =
  { roles: Role_set.t
  ; owner: unit t option
  ; uuid: Uuidm.t
  ; typ: 'a
  } [@@deriving eq,ord,show,yojson]

let make ~roles ~typ ?owner uuid =
  { roles
  ; owner
  ; uuid
  ; typ
  }

let serialize_roles t =
  let roles =
    List.map
      Base64.encode_exn
      (Role_set.elements t.roles)
  in
  String.concat "," roles

let a_owns_b a b =
  Option.map (fun b' -> a.uuid = b'.uuid) b.owner = Some true

let has_role t role =
  Role_set.mem role t.roles
