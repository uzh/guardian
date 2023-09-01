type t =
  | Create
  | Read
  | Update
  | Delete
  | Manage

val equal : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
val show : t -> string
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
val of_string : string -> t
val sexp_of_t : t -> Sexplib0.Sexp.t

(** [is_valid] checks the validity of the provided action [t] against action
    [matches] or action [Manage] *)
val is_valid : matches:t -> t -> bool
