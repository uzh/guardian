open CCFun.Infix
module Uuid = Guardian.Contract.Uuid

module Actor = struct
  type t =
    [ `User
    | `Admin
    | `Hacker
    | `Editor of Uuid.Target.t
    ]
  [@@deriving show, eq, ord, yojson]

  let name = show %> Guardian.Utils.decompose_variant_string %> fst

  let find_target = function
    | `User | `Admin | `Hacker -> None
    | `Editor x -> Some x
  ;;

  let find_target_exn = find_target %> CCOption.get_exn_or "No target"
  let all = [ `User; `Admin; `Hacker; `Editor Uuid.Target.nil ]

  let of_string =
    Guardian.Utils.decompose_variant_string
    %> function
    | "user", [] -> `User
    | "admin", [] -> `Admin
    | "hacker", [] -> `Hacker
    | "editor", [ id ] ->
      Logs.debug (fun m -> m "Parsing role `Editor with id: %s" id);
      `Editor (Uuid.Target.of_string_exn id)
    | role -> Guardian.Utils.failwith_invalid_role role
  ;;
end

module Target = struct
  type t =
    [ `User
    | `Article
    | `Post
    ]
  [@@deriving show, eq, ord, yojson]

  let name = show %> Guardian.Utils.decompose_variant_string %> fst

  let find_target = function
    | `User | `Article | `Post -> None
  ;;

  let find_target_exn = find_target %> CCOption.get_exn_or "No target"
  let all = [ `User; `Article; `Post ]

  let of_string s =
    match Guardian.Utils.decompose_variant_string s with
    | "user", [] -> `User
    | "article", [] -> `Article
    | "post", [] -> `Post
    | role -> Guardian.Utils.failwith_invalid_role role
  ;;
end
