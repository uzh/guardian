let ppx_printer m fmt _ = Format.pp_print_string fmt m

type t =
  | Create [@name "create"] [@printer ppx_printer "create"]
  | Read [@name "read"] [@printer ppx_printer "read"]
  | Update [@name "update"] [@printer ppx_printer "update"]
  | Delete [@name "delete"] [@printer ppx_printer "delete"]
  | Manage [@name "manage"] [@printer ppx_printer "manage"]
[@@deriving eq, ord, show { with_path = false }, yojson, sexp_of]

let of_string = function
  | "create" -> Some Create
  | "read" -> Some Read
  | "update" -> Some Update
  | "delete" -> Some Delete
  | "manage" -> Some Manage
  | _ -> None
;;

let of_string_exn s =
  CCOption.get_exn_or (Format.asprintf "Invalid action: %s" s) (of_string s)
;;

let of_string_res s =
  CCOption.to_result (Format.asprintf "Invalid action: %s" s) (of_string s)
;;

let is_valid ~required action = equal Manage action || equal required action
