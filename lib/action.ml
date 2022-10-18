type ('e, 'eo) t =
  [ `Create of 'e
  | `Read of 'eo
  | `Update of 'eo
  | `Delete of 'eo
  | `Manage of 'eo ]
[@@deriving ord, show]

(* let to_string = show *)
let to_string = function
  | `Create _ -> "`Create"
  | `Read _ -> "`Read"
  | `Update _ -> "`Update"
  | `Delete _ -> "`Delete"
  | `Manage _ -> "`Manage"

let of_string target s =
  match String.(trim s |> lowercase_ascii) with
  | "create" | "`create" -> `Create target
  | "read" | "`read" -> `Read target
  | "update" | "`update" -> `Update target
  | "delete" | "`delete" -> `Delete target
  | "manage" | "`manage" -> `Manage target
  | _ -> raise (Invalid_argument ("Invalid action: " ^ s))
