let ppx_printer m fmt _ = Format.pp_print_string fmt m

type t =
  | Create [@name "create"] [@printer ppx_printer "create"]
  | Read [@name "read"] [@printer ppx_printer "read"]
  | Update [@name "update"] [@printer ppx_printer "update"]
  | Delete [@name "delete"] [@printer ppx_printer "delete"]
  | Manage [@name "manage"] [@printer ppx_printer "manage"]
[@@deriving eq, ord, show { with_path = false }, yojson, sexp_of]

let of_string = function
  | "create" -> Create
  | "read" -> Read
  | "update" -> Update
  | "delete" -> Delete
  | "manage" -> Manage
  | act -> raise (Invalid_argument (Format.asprintf "Invalid action: %s" act))
;;

let is_valid ~matches action = equal Manage action || equal matches action
