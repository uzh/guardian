module Actor = struct
  type t =
    [ `User
    | `Admin
    | `Hacker
    | `Editor of Guardian.Uuid.Target.t
      [@equal fun a b -> Guardian.Uuid.Target.(equal a nil || equal b nil)]
    ]
  [@@deriving show, eq, ord, yojson]

  let name = function
    | `User -> "user"
    | `Admin -> "admin"
    | `Hacker -> "hacker"
    | `Editor _ -> "editor"
  ;;

  let find_target = function
    | `User | `Admin | `Hacker -> None
    | `Editor x -> Some x
  ;;

  let find_target_exn = CCFun.(find_target %> CCOption.get_exn_or "No target")
  let all = [ `User; `Admin; `Hacker; `Editor Guardian.Uuid.Target.nil ]

  let of_string s =
    match Guardian.Util.decompose_variant_string s with
    | "user", [] -> `User
    | "admin", [] -> `Admin
    | "hacker", [] -> `Hacker
    | "editor", [ id ] ->
      Logs.debug (fun m -> m "Parsing role string: %s" s);
      `Editor (Guardian.Uuid.Target.of_string_exn id)
    | _ -> failwith (Format.asprintf "Invalid role: %s" s)
  ;;
end

module Target = struct
  type t =
    [ `User
    | `Article
    ]
  [@@deriving show, eq, ord, yojson]

  let name = function
    | `User -> "user"
    | `Article -> "article"
  ;;

  let find_target = function
    | `User | `Article -> None
  ;;

  let find_target_exn = CCFun.(find_target %> CCOption.get_exn_or "No target")
  let all = [ `User; `Article ]

  let of_string s =
    match Guardian.Util.decompose_variant_string s with
    | "user", [] -> `User
    | "article", [] -> `Article
    | _ -> failwith (Format.asprintf "Invalid role: %s" s)
  ;;
end
