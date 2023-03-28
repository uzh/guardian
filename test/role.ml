open CCFun.Infix
module Uuid = Guardian.Contract.Uuid

module Actor = struct
  type t =
    [ `Admin
    | `Editor of Uuid.Target.t
    | `Hacker
    | `Reader of Uuid.Target.t
    | `User
    ]
  [@@deriving show, eq, ord, yojson]

  let name = show %> Guardian.Utils.decompose_variant_string %> fst

  let find_target = function
    | `Admin | `Hacker | `User -> None
    | `Editor x | `Reader x -> Some x
  ;;

  let find_target_exn = find_target %> CCOption.get_exn_or "No target"

  let all =
    [ `Admin; `Editor Uuid.Target.nil; `Hacker; `Reader Uuid.Target.nil; `User ]
  ;;

  let of_string =
    Guardian.Utils.decompose_variant_string
    %> function
    | "admin", [] -> `Admin
    | "editor", [ id ] ->
      Logs.debug (fun m -> m "Parsing role `Editor with id: %s" id);
      `Editor (Uuid.Target.of_string_exn id)
    | "hacker", [] -> `Hacker
    | "reader", [ id ] ->
      Logs.debug (fun m -> m "Parsing role `Reader with id: %s" id);
      `Reader (Uuid.Target.of_string_exn id)
    | "user", [] -> `User
    | role -> Guardian.Utils.failwith_invalid_role role
  ;;
end

module Target = struct
  type t =
    [ `Article
    | `Note
    | `Post
    | `User
    ]
  [@@deriving show, eq, ord, yojson]

  let name = show %> Guardian.Utils.decompose_variant_string %> fst

  let find_target = function
    | `Article | `Note | `Post | `User -> None
  ;;

  let find_target_exn = find_target %> CCOption.get_exn_or "No target"
  let all = [ `Article; `Note; `Post; `User ]

  let of_string s =
    match Guardian.Utils.decompose_variant_string s with
    | "article", [] -> `Article
    | "note", [] -> `Note
    | "post", [] -> `Post
    | "user", [] -> `User
    | role -> Guardian.Utils.failwith_invalid_role role
  ;;
end
