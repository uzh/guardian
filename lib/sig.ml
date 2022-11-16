module type Uuid = sig
  type t

  val nil : t
  val ns_dns : t
  val ns_url : t
  val ns_oid : t
  val ns_X500 : t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val of_bytes : ?pos:int -> string -> t option
  val to_bytes : t -> string
  val of_mixed_endian_bytes : ?pos:int -> string -> t option
  val to_mixed_endian_bytes : t -> string
  val unsafe_of_bytes : string -> t
  val unsafe_to_bytes : t -> string
  val of_string : ?pos:int -> string -> t option
  val to_string : ?upper:bool -> t -> string
  val pp : Format.formatter -> t -> unit
  val pp_string : ?upper:bool -> Format.formatter -> t -> unit
  val to_yojson : t -> [> `String of string ]
  val of_yojson : [> `String of string ] -> (t, string) result
  val of_string_exn : string -> t
  val create : unit -> t
end
