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

let global_perms =
  [ `Role "user", `Read, `Role "article"
  ; `Role "admin", `Create, `Role "article"
  ; `Role "admin", `Read, `Role "article"
  ; `Role "admin", `Update, `Role "article"
  ; `Role "admin", `Delete, `Role "article"
  ]

let ( let* ) = Result.bind

let test_create_entity () =
  Alcotest.(check (result unit string))
    "Create an entity."
    ( let* _aron_ent = User.to_entity aron in
      let* _chris_ent = User.to_entity chris in
      let* _ben_ent = Hacker.to_entity ben in
      let* _chris_art_ent = Article.to_entity chris_article in
      let* _aron_art_ent = Article.to_entity aron_article in
      Ok()
    )
    (Ok())

let test_grant_roles () =
  Alcotest.(check (result unit string))
    "Grant a role."
    (Ocauth_store.grant_roles (snd aron) (Ocaml_authorize.Role_set.singleton "admin"))
    (Ok())

let test_check_roles () =
  Alcotest.(check (result unit string))
    "Check a user's roles."
    ( let* roles = Ocauth_store.get_roles (snd aron) in
      let expected = Ocaml_authorize.Role_set.of_list ["user"; "admin"] in
      let diff =
        Ocaml_authorize.Role_set.(
          union (diff expected roles) (diff roles expected)
          |> elements
        )
      in
      if diff = []
      then Ok()
      else
        let open Ocaml_authorize.Role_set in
        let received = [%show: string list] (elements roles) in
        let expected = [%show: string list] (elements expected) in
        Error(
          Printf.sprintf
            "Got %s for roles of entity %s, but expected %s."
            received
            (Uuidm.to_string (snd aron))
            expected)
    )
    (Ok())

let test_push_perms () =
  Alcotest.(check bool)
    "Push global permissions."
    (Result.is_ok
       (Ocauth_store.put_perms global_perms))
    true

let test_read_perms () =
  Alcotest.(check (result unit string))
    "Read the global permissions we've just pushed."
    ( let* perms = Ocauth_store.get_perms (`Role "article") in
      let global_set = Ocaml_authorize.Authorizer.Auth_rule_set.of_list global_perms in
      let retrieved_set = Ocaml_authorize.Authorizer.Auth_rule_set.of_list perms in
      let diff = Ocaml_authorize.Authorizer.Auth_rule_set.diff global_set retrieved_set in
      let diff' =
        Ocaml_authorize.Authorizer.Auth_rule_set.elements diff
        |> [%show: Ocaml_authorize.Authorizer.auth_rule list]
      in
      if Ocaml_authorize.Authorizer.Auth_rule_set.compare global_set retrieved_set = 0
      then Ok ()
      else Error(Printf.sprintf "Permissions diff: %s." diff')
    )
    (Ok())

let test_update_owned () =
  Alcotest.(check bool)
    "Chris can update an article owned by Chris."
    (Result.is_ok
       ( let* chris_ent = User.to_entity chris in
         Article.update_title chris_ent chris_article "Updated Title"
       )
    )
    true

let test_admin_update_others' () =
  Alcotest.(check bool)
    "Aron (admin) can update an article Chris owns"
    (Result.is_ok
       ( let* aron_ent = User.to_entity aron in
         Article.update_title aron_ent chris_article "Updated Title"
       )
    )
    true

let cannot_update () =
  Alcotest.(check bool)
    "Chris cannot update an article Aron owns"
    (Result.is_error
       ( let* chris_ent = User.to_entity chris in
         Article.update_title chris_ent aron_article "Updated Title"))
    true

let can_update_self () =
  Alcotest.(check bool)
    "Article can update itself."
    (Result.is_ok
       ( let* chris_article_entity = Article.to_entity chris_article in
         Article.update_title chris_article_entity chris_article "Updated Title"))
    true

let article_cannot_update_other_article () =
  let () = print_endline "about to run a test" in
  Alcotest.(check bool)
    "Article cannot update another article."
    (Result.is_error
       ( let* chris_article_entity = Article.to_entity chris_article in
         Article.update_title chris_article_entity aron_article "Updated Title"))
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
    Alcotest.run "Authorization"
      [ ( "Managing entities."
        , [ Alcotest.test_case "Create an entity." `Quick test_create_entity
          ]
        )
      ; ( "Managing roles."
        , [ Alcotest.test_case "Grant a role." `Quick test_grant_roles
          ; Alcotest.test_case "Check roles." `Quick test_check_roles
          ]
        )
      ; ( "Managing authorization rules."
        , [ Alcotest.test_case "Push rules." `Quick test_push_perms
          ; Alcotest.test_case "Read rules." `Quick test_read_perms
          ]
        )
      ; ( "Admins should be able to do everything."
        , [ Alcotest.test_case "Update someone else's article." `Quick test_admin_update_others'
          ]
        )
      ; ( "An entity should be able to do everything to entities it owns."
        , [ Alcotest.test_case "Update own article." `Quick test_update_owned
          ]
        )
      ; ( "An entity should be able to do everything to itself."
        , [ Alcotest.test_case "Update" `Quick can_update_self
          ]
        )
      ; ( "Entities should be denied access to entities they shouldn't access."
        , [ Alcotest.test_case "Cannot update" `Quick cannot_update
          ; Alcotest.test_case "Cannot update" `Quick article_cannot_update_other_article
            (* ; Alcotest.test_case "Cannot update" `Quick hacker_cannot_update_article *)
          ]
        )
      ]
  in
  Ok ()

let () =
  if Sqlite3.db_close Ocauth_store.db
  then
    match return with
    | Ok _ -> print_endline "returned successfully"
    | Error s -> print_endline("Error: " ^ s)
  else
    print_endline "Failed to close db"
