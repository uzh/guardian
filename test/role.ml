type t =
  [ `User
  | `Admin
  | `Article
  | `Hacker
  | `Editor of Guardian.Uuid.Target.t
    [@equal fun a b -> Guardian.Uuid.Target.(equal a nil || equal b nil)]
  ]
[@@deriving show, eq, ord, yojson]

let get_name = function
  | `User -> "user"
  | `Admin -> "admin"
  | `Article -> "article"
  | `Hacker -> "hacker"
  | `Editor _ -> "editor"
;;

let get_target = function
  | `User | `Admin | `Article | `Hacker -> failwith "No target"
  | `Editor x -> x
;;

let all = [ `User; `Admin; `Article; `Hacker; `Editor Guardian.Uuid.Target.nil ]

let of_string s =
  match Guardian.Util.decompose_variant_string s with
  | "user", [] -> `User
  | "admin", [] -> `Admin
  | "article", [] -> `Article
  | "hacker", [] -> `Hacker
  | "editor", [ id ] ->
    let () = Printf.printf "Parsing role string: %s\n" s in
    `Editor (Guardian.Uuid.Target.of_string_exn id)
  | _ -> failwith ("Invalid role: " ^ s)
;;
