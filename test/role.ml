open CCFun.Infix

module Actor = struct
  type t =
    [ `User
    | `Admin
    | `Hacker
    | `Editor of Guardian.Uuid.Target.t
      [@equal fun a b -> Guardian.Uuid.Target.(equal a nil || equal b nil)]
    ]
  [@@deriving show, eq, ord, yojson]

  let name = show %> Guardian.Utils.decompose_variant_string %> fst

  let find_target = function
    | `User | `Admin | `Hacker -> None
    | `Editor x -> Some x
  ;;

  let find_target_exn = find_target %> CCOption.get_exn_or "No target"
  let all = [ `User; `Admin; `Hacker; `Editor Guardian.Uuid.Target.nil ]

  let of_string s =
    match Guardian.Utils.decompose_variant_string s with
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
    | _ -> failwith (Format.asprintf "Invalid role: %s" s)
  ;;
end
