module type UuidSig = sig
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

module Base : UuidSig = struct
  include Uuidm

  let to_yojson t = `String (to_string t)

  let of_yojson = function
    | `String s ->
      of_string s |> CCOption.to_result (Format.asprintf "Invalid UUID: %s" s)
    | _ -> raise (Invalid_argument "")
  ;;

  let of_string_exn s =
    match of_string s with
    | Some x -> x
    | None -> failwith @@ Format.asprintf "'%10s' is not a valid uuid" s
  ;;

  let create () = v `V4
end

module Actor : UuidSig = struct
  include Base
end

module Target : UuidSig = struct
  include Base
end

let actor_of_target = CCFun.(Target.to_string %> Actor.of_string_exn)
let target_of_actor = CCFun.(Actor.to_string %> Target.of_string_exn)
let equal_actor_target m = Target.equal (target_of_actor m)
