type t = string [@@deriving show, eq, ord]
(** representation is simply the group's name *)

let of_yojson : Yojson.Safe.t -> (t, string) Result.t = function
  | `String s -> Ok s
  | _ -> Error "Invalid role."

module type S = sig
  type t [@@deriving show, eq, ord, yojson]

  val get_name : t -> string
  val get_target : t -> Uuidm.t
  val of_string : string -> t
  val all : t list
end
