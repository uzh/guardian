type t =
  [ `User
  | `Admin
  | `Article
  | `Hacker
  ]
  [@@deriving show,eq,ord,yojson]

let all = [ `User; `Admin; `Article; `Hacker ]

let of_string s =
  match String.(trim (lowercase_ascii s)) with
  | "user" | "`user" -> `User
  | "admin" | "`admin" -> `Admin
  | "article" | "`article" -> `Article
  | "hacker" | "`hacker" -> `Hacker
  | _ -> failwith("Invalid role: " ^ s)
