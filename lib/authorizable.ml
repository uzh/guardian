module type S = sig
  type role_set
  type role

  type 'a t =
    { roles: role_set
    ; owner: unit t option
    ; uuid: Uuidm.t
    ; typ: 'a
    } [@@deriving eq,ord,show,yojson]

  val make : roles:role_set -> typ:'a -> ?owner:unit t -> Uuid.t -> 'a t

  val a_owns_b : 'a t -> 'a t -> bool

  val has_role : 'a t -> role -> bool
end

module Make(RS : Role_set.S) : (S with type role := RS.elt and type role_set := RS.t) = struct
  type 'a t =
    { roles: RS.t
    ; owner: unit t option
    ; uuid: Uuid.t
    ; typ: 'a
    } [@@deriving eq,ord,show,yojson]

  let make ~roles ~typ ?owner uuid =
    { roles
    ; owner
    ; uuid
    ; typ
    }

  let a_owns_b a b =
    Option.map (fun b' -> a.uuid = b'.uuid) b.owner = Some true

  let has_role t role =
    RS.mem role t.roles
end