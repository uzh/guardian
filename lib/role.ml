(** representation is simply the group's name *)
type t = string [@@deriving show, eq, ord]

let of_yojson : Yojson.Safe.t -> (t, string) CCResult.t = function
  | `String s -> Ok s
  | _ -> Error "Invalid role."
;;

module type S = sig
  type t [@@deriving show, eq, ord, yojson]

  val get_name : t -> string
  val get_target : t -> Uuid.Target.t
  val of_string : string -> t
  val all : t list
end
