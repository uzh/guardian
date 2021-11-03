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

let () =
  print_endline "Users:";
  print_endline (User.show chris);
  print_endline (User.show aron);
  print_endline (User.show ben);
  print_endline "Articles:";
  print_endline (Article.show chris_article);
  print_endline (Article.show aron_article)

let ( let* ) = Result.bind

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
  let* () = Ocauth_store.grant_roles (snd aron) (Ocaml_authorize.Role_set.singleton "admin") in
  let* () =
    match
      Ocauth_store.put_perms
        [ `Role "user", `Read, `Role "article"
        ; `Role "admin", `Create, `Role "article"
        ; `Role "admin", `Read, `Role "article"
        ; `Role "admin", `Update, `Role "article"
        ; `Role "admin", `Delete, `Role "article"
        ]
    with
    | Ok perms ->
      let () = print_endline "Successfully put some perms: " in
      let () = print_endline ([%show: Ocaml_authorize.Authorizer.auth_rule list] perms) in
      Ok ()
    | Error _ ->
      Error "Failed to put some perms"
  in
  let _ =
    Alcotest.run "Authorization"
      [ ( "Admins should be able to do everything."
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
