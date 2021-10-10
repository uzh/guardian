type t =
  { roles: Role_set.t
  ; owner: t option
  ; uuid: Uuidm.t
  } [@@deriving eq]

let make ~roles ?owner uuid =
  { roles
  ; owner
  ; uuid
  }

let serialize_roles t =
  let roles =
    List.map
      Base64.encode_exn
      (Role_set.elements t.roles)
  in
  String.concat "," roles

let a_owns_b a b =
  Option.map (equal a) b.owner = Some true

let has_role t role =
  Role_set.mem role t.roles
