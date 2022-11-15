(** representation is simply the group's name *)
type t = string [@@deriving show, eq, ord]

let of_yojson : Yojson.Safe.t -> (t, string) CCResult.t = function
  | `String s -> Ok s
  | _ -> Error "Invalid role."
;;

module type RoleSig = sig
  type t [@@deriving eq, show, ord, yojson]

  val name : t -> string
  val find_target : t -> Uuid.Target.t option
  val find_target_exn : t -> Uuid.Target.t
  val of_string : string -> t
  val all : t list
end
