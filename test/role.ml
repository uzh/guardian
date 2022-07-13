type t =
  [ `User
  | `Admin
  | `Article
  | `Hacker
  | `Editor of Ocaml_authorize.Uuidm.t [@equal fun a b ->
       Uuidm.equal a nil || Uuidm.equal b nil ]
  ]
  [@@deriving show,eq,ord,yojson]

let get_name = function
  | `User -> "user"
  | `Admin -> "admin"
  | `Article -> "article"
  | `Hacker -> "hacker"
  | `Editor _ -> "editor"

let get_target = function
  | `User
  | `Admin
  | `Article
  | `Hacker -> failwith "No target"
  | `Editor x -> x

let all = [ `User; `Admin; `Article; `Hacker; `Editor (Uuidm.nil) ]

let of_string s =
  match Ocaml_authorize.Util.decompose_variant_string s with
  | "user", [] -> `User
  | "admin", [] -> `Admin
  | "article", [] -> `Article
  | "hacker", [] -> `Hacker
  | "editor", [id] -> 
    let () = Printf.printf "Parsing role string: %s\n" s in
    `Editor(Ocaml_authorize.Uuidm.of_string_exn id)
  | _ -> failwith("Invalid role: " ^ s)
