open CCFun.Infix

module Role = struct
  type t =
    [ `Admin
    | `Author
    | `Editor
    | `Reader
    ]
  [@@deriving show, eq, ord, yojson, sexp_of]

  let name = show %> Guardian.Utils.decompose_variant_string %> fst

  let of_string_res =
    Guardian.Utils.decompose_variant_string
    %> function
    | "admin", [] -> Ok `Admin
    | "author", [] -> Ok `Author
    | "editor", [] -> Ok `Editor
    | "reader", [] -> Ok `Reader
    | role -> Error (Guardian.Utils.invalid_role role)
  ;;

  let of_string = of_string_res %> CCResult.get_or_failwith
  let all = [ `Admin; `Author; `Editor; `Reader ]
end

module Actor = struct
  type t =
    [ `Admin
    | `Hacker
    | `User
    ]
  [@@deriving show, eq, ord, yojson, sexp_of]

  let name = show %> Guardian.Utils.decompose_variant_string %> fst

  let of_string_res =
    Guardian.Utils.decompose_variant_string
    %> function
    | "admin", [] -> Ok `Admin
    | "hacker", [] -> Ok `Hacker
    | "user", [] -> Ok `User
    | role -> Error (Guardian.Utils.invalid_role role)
  ;;

  let of_string = of_string_res %> CCResult.get_or_failwith
  let all = [ `Admin; `Hacker; `User ]
end

module Target = struct
  type t =
    [ `Article
    | `Note
    | `Post
    | `User
    ]
  [@@deriving show, eq, ord, yojson, sexp_of]

  let name = show %> Guardian.Utils.decompose_variant_string %> fst

  let of_string_res =
    Guardian.Utils.decompose_variant_string
    %> function
    | "article", [] -> Ok `Article
    | "note", [] -> Ok `Note
    | "post", [] -> Ok `Post
    | "user", [] -> Ok `User
    | role -> Error (Guardian.Utils.invalid_role role)
  ;;

  let of_string = of_string_res %> CCResult.get_or_failwith
  let all = [ `Article; `Note; `Post; `User ]
end
