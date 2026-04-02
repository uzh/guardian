open CCFun.Infix

module Role = struct
  type t =
    [ `Admin
    | `Author
    | `Editor
    | `Reader
    ]
  [@@deriving show, eq, ord, yojson, sexp_of]

  let name = show %> Guardian.Utils.decompose_variant_string_exn %> fst

  let of_string_res =
    Guardian.Utils.decompose_variant_string
    %> function
    | Some ("admin", []) -> Ok `Admin
    | Some ("author", []) -> Ok `Author
    | Some ("editor", []) -> Ok `Editor
    | Some ("reader", []) -> Ok `Reader
    | Some role -> Error (Guardian.Utils.invalid_role role)
    | None -> Error (Guardian.Utils.invalid_role ("invalid format", []))
  ;;

  let of_string = of_string_res %> CCResult.to_option
  let of_string_exn = of_string_res %> CCResult.get_or_failwith
  let all = [ `Admin; `Author; `Editor; `Reader ]
end

module Actor = struct
  type t =
    [ `Admin
    | `Hacker
    | `User
    ]
  [@@deriving show, eq, ord, yojson, sexp_of]

  let name = show %> Guardian.Utils.decompose_variant_string_exn %> fst

  let of_string_res =
    Guardian.Utils.decompose_variant_string
    %> function
    | Some ("admin", []) -> Ok `Admin
    | Some ("hacker", []) -> Ok `Hacker
    | Some ("user", []) -> Ok `User
    | Some role -> Error (Guardian.Utils.invalid_role role)
    | None -> Error (Guardian.Utils.invalid_role ("invalid format", []))
  ;;

  let of_string = of_string_res %> CCResult.to_option
  let of_string_exn = of_string_res %> CCResult.get_or_failwith
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

  let name = show %> Guardian.Utils.decompose_variant_string_exn %> fst

  let of_string_res =
    Guardian.Utils.decompose_variant_string
    %> function
    | Some ("article", []) -> Ok `Article
    | Some ("note", []) -> Ok `Note
    | Some ("post", []) -> Ok `Post
    | Some ("user", []) -> Ok `User
    | Some role -> Error (Guardian.Utils.invalid_role role)
    | None -> Error (Guardian.Utils.invalid_role ("invalid format", []))
  ;;

  let of_string = of_string_res %> CCResult.to_option
  let of_string_exn = of_string_res %> CCResult.get_or_failwith
  let all = [ `Article; `Note; `Post; `User ]
end

open Alcotest

let test_invalid_role_string () =
  let invalids = [ "superadmin"; ""; "admin,extra"; "123" ] in
  List.iter
    (fun s ->
       match Role.of_string_res s with
       | Ok _ -> fail "Expected error for invalid role string"
       | Error _ -> ())
    invalids
;;

let test_invalid_actor_string () =
  let invalids = [ "ghost"; ""; "admin,extra"; "user1" ] in
  List.iter
    (fun s ->
       match Actor.of_string_res s with
       | Ok _ -> fail "Expected error for invalid actor string"
       | Error _ -> ())
    invalids
;;

let test_invalid_target_string () =
  let invalids = [ "thing"; ""; "article,extra"; "note1" ] in
  List.iter
    (fun s ->
       match Target.of_string_res s with
       | Ok _ -> fail "Expected error for invalid target string"
       | Error _ -> ())
    invalids
;;

let () =
  run
    "Role/Actor/Target string parsing"
    [ ( "invalid role string"
      , [ test_case "invalid role" `Quick test_invalid_role_string ] )
    ; ( "invalid actor string"
      , [ test_case "invalid actor" `Quick test_invalid_actor_string ] )
    ; ( "invalid target string"
      , [ test_case "invalid target" `Quick test_invalid_target_string ] )
    ]
;;
