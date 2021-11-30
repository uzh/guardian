type t = [
    `Create |
    `Read |
    `Update |
    `Delete |
    `Manage
] [@@deriving ord,show]

let to_string = show

let of_string s =
  match String.(trim s |> lowercase_ascii) with
  | "create" | "`create" -> `Create
  | "read" | "`read" -> `Read
  | "update" | "`update" -> `Update
  | "delete" | "`delete" -> `Delete
  | "manage" | "`manage" -> `Manage
  | _ -> raise (Invalid_argument("Invalid action: " ^ s))
