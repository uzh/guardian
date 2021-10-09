type t =
  { roles: RoleSet.t
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

let serializeRoles t =
  let roles =
    List.map
      Base64.encode_exn
      (RoleSet.elements t.roles)
  in
  String.concat "," roles

let aOwnsB a b =
  Option.map (equal a) b.owner = Some true

let hasRole t role =
  RoleSet.exists (Role.equal role) t.roles
