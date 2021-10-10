type t =
  { roles: Role_set.t
  ; owner: t option
  ; hash: string option
  } [@@deriving eq]

let make ~roles ?owner ?repr () =
  { roles
  ; owner
  ; hash =
      match repr with
      | None -> None
      | Some x -> Some (Printf.sprintf "%08x" (Hashtbl.hash x))
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
