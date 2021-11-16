module Backend = Ocaml_authorize_backends.Mariadb_backend

module Article = Article.Make(Backend)

module Hacker = Hacker.Make(Backend)

module User = User.Make(Backend)

(* ensure that the `User` module conforms to the `Authorizable_entity` module type. *)
let _ = (module User : Ocaml_authorize.Authorizer.Authorizable_entity)

let _ = (module Article : Ocaml_authorize.Authorizer.Authorizable_entity)

let chris = "Chris", Uuidm.create `V4
let aron = "Aron", Uuidm.create `V4
let ben : Hacker.t = "Ben Hackerman", Uuidm.create `V4
let chris_article =
  Article.make
    ~title:"Foo"
    ~content:"Bar"
    ~uuid:(Uuidm.create `V4)
    ~author:chris
let aron_article =
  Article.make
    ~title:"Fizz"
    ~content:"Buzz"
    ~uuid:(Uuidm.create `V4)
    ~author:aron

let bad_perm = `Uniq (snd chris), `Update, `Uniq (aron_article.uuid)

let global_perms: Ocaml_authorize.Authorizer.auth_rule list =
  [ `Role "user", `Read, `Role "article"
  ; `Role "admin", `Create, `Role "article"
  ; `Role "admin", `Read, `Role "article"
  ; `Role "admin", `Update, `Role "article"
  ; `Role "admin", `Delete, `Role "article"
  ]

let ( let* ) = Result.bind

let (>|=) = Lwt.Infix.(>|=)

let test_create_entity _ () =
  ( let ( let* ) = Lwt_result.bind in
    let* _aron_ent = User.to_entity aron in
    let* _chris_ent = User.to_entity chris in
    let* _ben_ent = Hacker.to_entity ben in
    let* _chris_art_ent = Article.to_entity chris_article in
    let* _aron_art_ent = Article.to_entity aron_article in
    Lwt.return_ok()
  )
  >|=
  Alcotest.(check (result unit string))
    "Create an entity."
    (Ok())

let test_get_entity _ () =
  ( match%lwt Backend.get_entity ~typ:`User (snd aron) with
    | Ok(_) -> Lwt.return_true
    | Error _ -> Lwt.return_false
  )
  >|=
  Alcotest.(check bool)
    "Fetch an entity."
    true

let test_grant_roles _ () =
  (Backend.grant_roles (snd aron) (Ocaml_authorize.Role_set.singleton "admin"))
  >|=
  Alcotest.(check (result unit string))
    "Grant a role."
    (Ok())

let test_check_roles _ () =
  ( let ( let* ) = Lwt_result.bind in
    let* roles = Backend.get_roles (snd aron) in
    let expected = Ocaml_authorize.Role_set.of_list ["user"; "admin"] in
    let diff =
      Ocaml_authorize.Role_set.(
        union (diff expected roles) (diff roles expected)
        |> elements
      )
    in
    if diff = []
    then Lwt.return_ok()
    else
      let open Ocaml_authorize.Role_set in
      let received = [%show: string list] (elements roles) in
      let expected = [%show: string list] (elements expected) in
      Lwt.return_error(
        Printf.sprintf
          "Got %s for roles of entity %s, but expected %s."
          received
          (Uuidm.to_string (snd aron))
          expected)
  )
  >|=
  Alcotest.(check (result unit string))
    "Check a user's roles."
    (Ok())

let test_push_perms _ () =
  ( let%lwt res = Backend.put_perms global_perms in
    Lwt.return(Result.is_ok res)
  )
  >|=
  Alcotest.(check bool)
    "Push global permissions."
    true

let test_read_perms _ () =
  ( let ( let* ) = Lwt_result.bind in
    let* perms = Backend.get_perms (`Role "article") in
    let global_set = Ocaml_authorize.Authorizer.Auth_rule_set.of_list global_perms in
    let retrieved_set = Ocaml_authorize.Authorizer.Auth_rule_set.of_list perms in
    let diff = Ocaml_authorize.Authorizer.Auth_rule_set.diff global_set retrieved_set in
    let diff' =
      Ocaml_authorize.Authorizer.Auth_rule_set.elements diff
      |> [%show: Ocaml_authorize.Authorizer.auth_rule list]
    in
    if Ocaml_authorize.Authorizer.Auth_rule_set.compare global_set retrieved_set = 0
    then Lwt.return_ok()
    else Lwt.return_error(Printf.sprintf "Permissions diff: %s." diff')
  )
  >|=
  Alcotest.(check (result unit string))
    "Read the global permissions we've just pushed."
    (Ok())
let test_drop_perms _ () =
  ( let ( let* ) = Lwt_result.bind in
    let* () = Backend.put_perm bad_perm in
    let* perms = Backend.get_perms (`Uniq aron_article.uuid) in
    let* () =
      match perms with
      | [perm] ->
        if perm = bad_perm
        then Lwt.return_ok()
        else Lwt.return_error "Failed to push bad permission to test perm dropping."
      | _ ->
        Lwt.return_error "Invalid permissions."
    in
    let* () = Backend.delete_perm bad_perm in
    let* perms' = Backend.get_perms (`Uniq aron_article.uuid) in
    match perms' with
    | [] -> Lwt.return_ok()
    | _ -> Lwt.return_error "Failed to remove bad perm."
  )
  >|=
  Alcotest.(check (result unit string))
    "Read the global permissions we've just pushed."
    (Ok())

let test_update_owned _ () =
  ( let%lwt chris_ent = User.to_entity chris in
    match chris_ent with
    | Ok chris_ent ->
      let%lwt res = Article.update_title chris_ent chris_article "Updated Title" in
      Lwt.return(Result.is_ok res)
    | Error _ ->
      Lwt.return false
  )
  >|=
  Alcotest.(check bool)
    "Chris can update an article owned by Chris."
    true

let test_admin_update_others' _ () =
  ( let%lwt aron_ent = User.to_entity aron in
    match aron_ent with
    | Ok aron_ent ->
      let%lwt res = Article.update_title aron_ent chris_article "Updated Title" in
      Lwt.return(Result.is_ok res)
    | Error _ ->
      Lwt.return false
  )
  >|=
  Alcotest.(check bool)
    "Aron (admin) can update an article Chris owns"
    true

let cannot_update _ () =
  ( let%lwt chris_ent = User.to_entity chris in
    match chris_ent with
    | Ok chris_ent ->
      let%lwt res = Article.update_title chris_ent aron_article "Updated Title" in
      Lwt.return(Result.is_error res)
    | Error _ ->
      Lwt.return false
  )
  >|=
  Alcotest.(check bool)
    "Chris cannot update an article Aron owns"
    true

let can_update_self _ () =
  ( let%lwt chris_article_entity = Article.to_entity chris_article in
    match chris_article_entity with
    | Ok chris_article_entity ->
      let%lwt res = Article.update_title chris_article_entity chris_article "Updated Title" in
      Lwt.return(Result.is_ok res)
    | Error _ ->
      Lwt.return_false
  )
  >|=
  Alcotest.(check bool)
    "Article can update itself."
    true

let article_cannot_update_other_article _ () =
  ( let%lwt chris_article_entity = Article.to_entity chris_article in
    match chris_article_entity with
    | Ok chris_article_entity ->
      let%lwt res = Article.update_title chris_article_entity aron_article "Updated Title" in
      Lwt.return(Result.is_error res)
    | Error _ ->
      Lwt.return_false
  )
  >|=
  Alcotest.(check bool)
    "Article cannot update another article."
    true

(** IMPORTANT: the following tests should not compile! *)
(* let hacker_cannot_update_article () =
   let () = print_endline "about to run a test" in
   Alcotest.(check bool)
    "Article cannot update another article."
    (Result.is_error
       (Article.update_title (Hacker.to_entity ben) aron_article "Updated Title"))
    true

   let owner_can_do_nothing () =
   Alcotest.(check bool)
    "entity.owner shouldn't be allowed to do anything."
    (Result.is_error
       (Article.update_title (Article.to_entity aron_article).owner aron_article "Updated Title"))
    true *)

let return =
  let () =
    Lwt_main.run @@ Alcotest_lwt.run "Authorization"
      [ ( "Managing entities."
        , [ Alcotest_lwt.test_case "Create an entity." `Quick test_create_entity
          ; Alcotest_lwt.test_case "Retrieve an entity." `Quick test_get_entity
          ]
        )
      ; ( "Managing roles."
        , [ Alcotest_lwt.test_case "Grant a role." `Quick test_grant_roles
          ; Alcotest_lwt.test_case "Check roles." `Quick test_check_roles
          ]
        )
      ; ( "Managing authorization rules."
        , [ Alcotest_lwt.test_case "Push rules." `Quick test_push_perms
          ; Alcotest_lwt.test_case "Drop rules." `Quick test_drop_perms
          ; Alcotest_lwt.test_case "Read rules." `Quick test_read_perms
          ]
        )
      ; ( "Admins should be able to do everything."
        , [ Alcotest_lwt.test_case "Update someone else's article." `Quick test_admin_update_others'
          ]
        )
      ; ( "An entity should be able to do everything to entities it owns."
        , [ Alcotest_lwt.test_case "Update own article." `Quick test_update_owned
          ]
        )
      ; ( "An entity should be able to do everything to itself."
        , [ Alcotest_lwt.test_case "Update" `Quick can_update_self
          ]
        )
      ; ( "Entities should be denied access to entities they shouldn't access."
        , [ Alcotest_lwt.test_case "Cannot update" `Quick cannot_update
          ; Alcotest_lwt.test_case "Cannot update" `Quick article_cannot_update_other_article
            (* ; Alcotest.test_case "Cannot update" `Quick hacker_cannot_update_article *)
          ]
        )
      ]
  in
  Ok ()

(* let () =
  if Sqlite3.db_close Ocaml_authorize_backends.Sqlite3_backend.db
  then
    match return with
    | Ok _ -> print_endline "returned successfully"
    | Error s -> print_endline("Error: " ^ s)
  else
    print_endline "Failed to close db" *)
