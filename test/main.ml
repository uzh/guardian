(* ensure that the `User` module conforms to the `Authorizable_entity` module type. *)
let _ = (module User : Ocaml_authorize.Authorizer.Authorizable_entity)

let _ = (module Article : Ocaml_authorize.Authorizer.Authorizable_entity)

let chris = "Chris", Uuidm.create `V4
let aron = "Aron", Uuidm.create `V4
let ben: Hacker.t = "Ben Hackerman", Uuidm.create `V4
let chris_article = Article.make ~title:"Foo" ~content:"Bar" ~author:chris
let aron_article = Article.make ~title:"Fizz" ~content:"Buzz" ~author:aron

let test_update_owned () =
  Alcotest.(check bool)
    "Chris can update an article owned by Chris."
    (Result.is_ok
       (Article.update_title (User.to_entity chris) chris_article "Updated Title"))
    true

let test_admin_update_others' () =
  Alcotest.(check bool)
    "Aron (admin) can update an article Chris owns"
    (Result.is_ok
       (Article.update_title (User.to_entity aron) chris_article "Updated Title"))
    true

let cannot_update () =
  Alcotest.(check bool)
    "Chris cannot update an article Aron owns"
    (Result.is_error
       (Article.update_title (User.to_entity chris) aron_article "Updated Title"))
    true

let can_update_self () =
  Alcotest.(check bool)
    "Article can update itself."
    (Result.is_ok
       (Article.update_title (Article.to_entity chris_article) chris_article "Updated Title"))
    true

let article_cannot_update_other_article () =
  let () = print_endline "about to run a test" in
  Alcotest.(check bool)
    "Article cannot update another article."
    (Result.is_error
       (Article.update_title (Article.to_entity chris_article) aron_article "Updated Title"))
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

let () =
  let () = print_endline "About to run alcotest." in
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
