type t = [
    `Create |
    `Read |
    `Update |
    `Delete
] [@@deriving ord,show]

let to_string = show

let of_string s =
  match String.lowercase_ascii s with
  | "create" -> `Create
  | "read" -> `Read
  | "update" -> `Update
  | "delete" -> `Delete
  | _ -> raise (Invalid_argument("Invalid action: " ^ s))

let manage = [`Create; `Read; `Update; `Delete]
