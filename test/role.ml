open CCFun.Infix

module Role = struct
  type t =
    [ `Admin
    | `Author
    | `Editor
    | `Reader
    ]
  [@@deriving show, eq, ord, yojson]

  let name = show %> Guardian.Utils.decompose_variant_string %> fst

  let of_string =
    Guardian.Utils.decompose_variant_string
    %> function
    | "admin", [] -> `Admin
    | "author", [] -> `Author
    | "editor", [] -> `Editor
    | "reader", [] -> `Reader
    | role -> Guardian.Utils.failwith_invalid_role role
  ;;

  let all = [ `Admin; `Author; `Editor; `Reader ]
end

module Actor = struct
  type t =
    [ `Admin
    | `Hacker
    | `User
    ]
  [@@deriving show, eq, ord, yojson]

  let name = show %> Guardian.Utils.decompose_variant_string %> fst

  let of_string =
    Guardian.Utils.decompose_variant_string
    %> function
    | "admin", [] -> `Admin
    | "hacker", [] -> `Hacker
    | "user", [] -> `User
    | role -> Guardian.Utils.failwith_invalid_role role
  ;;

  let all = [ `Admin; `Hacker; `User ]
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

  let of_string s =
    match Guardian.Utils.decompose_variant_string s with
    | "article", [] -> `Article
    | "note", [] -> `Note
    | "post", [] -> `Post
    | "user", [] -> `User
    | role -> Guardian.Utils.failwith_invalid_role role
  ;;

  let all = [ `Article; `Note; `Post; `User ]
end
