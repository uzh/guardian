module type Uuid = sig
  type t

  val nil : t
  val max : t
  val ns_dns : t
  val ns_url : t
  val ns_oid : t
  val ns_X500 : t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val of_binary_string : ?pos:int -> string -> t option
  val to_binary_string : t -> string
  val unsafe_of_binary_string : string -> t
  val unsafe_to_binary_string : t -> string
  val of_string : ?pos:int -> string -> t option
  val to_string : ?upper:bool -> t -> string
  val pp : Format.formatter -> t -> unit
  val pp' : upper:bool -> Format.formatter -> t -> unit
  val to_yojson : t -> [> `String of string ]
  val of_yojson : [> `String of string ] -> (t, string) result
  val of_string_exn : string -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val create : unit -> t
end
