type t =
  { roles: Role.t list
  ; owner: t option
  ; hash: string option
  } [@@deriving show,eq]

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
      t.roles
  in
  String.concat "," roles

let aOwnsB a b =
  Option.map (equal a) b.owner = Some true

let hasRole t role =
  List.exists (Role.equal role) t.roles
