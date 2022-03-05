module Ocaml_authorize = Ocaml_authorize.Make(Role)

module Tests(Backend : Ocauth.Persistence_s) = struct
  module Article = Article.Make(Backend)

  module Hacker = Hacker.Make(Backend)

  module User = User.Make(Backend)

  (* ensure that the `User` module conforms to the `Authorizable_module` module type. *)
  let _ = (module User : Ocaml_authorize.Authorizer.Authorizable_module)

  let _ = (module Article : Ocaml_authorize.Authorizer.Authorizable_module)

  let chris = "Chris", Uuidm.v `V4
  let aron = "Aron", Uuidm.v `V4
  let ben : Hacker.t = "Ben Hackerman", Uuidm.v `V4
  let chris_article =
    Article.make
      ~title:"Foo"
      ~content:"Bar"
      ~uuid:(Uuidm.v `V4)
      ~author:chris
  let aron_article =
    Article.make
      ~title:"Fizz"
      ~content:"Buzz"
      ~uuid:(Uuidm.v `V4)
      ~author:aron

  let bad_perm = `Uniq (snd chris), `Update, `Uniq (aron_article.uuid)

  let global_perms: Ocaml_authorize.Authorizer.auth_rule list =
    [ `Role `User, `Read, `Role `Article
    ; `Role `Admin, `Manage, `Role `Article
    ]

  let ( let* ) = Lwt_result.bind

  let (>|=) = Lwt.Infix.(>|=)

  let test_create_authorizable _ () =
    ( let* _aron_ent = User.to_authorizable aron in
      let* _chris_ent = User.to_authorizable chris in
      let* _ben_ent = Hacker.to_authorizable ben in
      let* _chris_art_ent = Article.to_authorizable chris_article in
      let* _aron_art_ent = Article.to_authorizable aron_article in
      (* now we check to see that the authorizables have had ownership set *)
      let get_owner_id article =
        let* x = Backend.get_owner article.Article.uuid in
        Option.to_result ~none:("Couldn't get owner for article " ^ Article.show article) x
        |> Lwt.return
      in
      let* chris_art_owner = get_owner_id chris_article in
      let* aron_art_owner = get_owner_id aron_article in
      Lwt.return_ok(Uuidm.to_string chris_art_owner, Uuidm.to_string aron_art_owner)
    )
    >|=
    Alcotest.(check (result (pair string string) string))
      "Create an authorizable."
      (Ok(Uuidm.to_string(snd chris), Uuidm.to_string(snd aron)))

  let test_get_authorizable _ () =
    ( match%lwt Backend.get_authorizable ~typ:`User (snd aron) with
      | Ok(_) -> Lwt.return_true
      | Error err -> raise(Failure err)
    )
    >|=
    Alcotest.(check bool)
      "Fetch an authorizable."
      true

  let test_grant_roles _ () =
    (Backend.grant_roles (snd aron) (Ocaml_authorize.Role_set.singleton `Admin))
    >|=
    Alcotest.(check (result unit string))
      "Grant a role."
      (Ok())

  let test_check_roles _ () =
    ( let* roles = Backend.get_roles (snd aron) in
      let expected = Ocaml_authorize.Role_set.of_list [`User; `Admin] in
      let diff =
        Ocaml_authorize.Role_set.(
          union (diff expected roles) (diff roles expected)
          |> elements
        )
      in
      if diff = []
      then Lwt.return_ok()
      else
        let open Ocauth.Role_set in
        let received = [%show: Role.t list] (elements roles) in
        let expected = [%show: Role.t list] (elements expected) in
        Lwt.return_error(
          Printf.sprintf
            "Got %s for roles of authorizable %s, but expected %s."
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
    ( let* perms = Backend.get_perms (`Role `Article) in
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
    ( let* () = Backend.put_perm bad_perm in
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
    ( let* chris_ent = User.to_authorizable chris in
      let* _art = Article.update_title chris_ent chris_article "Updated Title" in
      Lwt.return_ok true
    )
    >|=
    Alcotest.(check (result bool string))
      "Chris can update an article owned by Chris."
      (Ok true)

  let test_admin_update_others' _ () =
    ( let%lwt aron_ent = User.to_authorizable aron in
      match aron_ent with
      | Ok aron_ent ->
        let%lwt res = Article.update_title aron_ent chris_article "Updated Title" in
        Lwt.return(Result.is_ok res)
      | Error err ->
        raise(Failure err)
    )
    >|=
    Alcotest.(check bool)
      "Aron (admin) can update an article Chris owns"
      true

  let cannot_update _ () =
    ( let%lwt chris_ent = User.to_authorizable chris in
      match chris_ent with
      | Ok chris_ent ->
        let%lwt res = Article.update_title chris_ent aron_article "Updated Title" in
        Lwt.return(Result.is_error res)
      | Error err ->
        raise(Failure err)
    )
    >|=
    Alcotest.(check bool)
      "Chris cannot update an article Aron owns"
      true

  let can_update_self _ () =
    ( let%lwt chris_article' = Article.to_authorizable chris_article in
      match chris_article' with
      | Ok chris_article' ->
        let%lwt res = Article.update_title chris_article' chris_article "Updated Title" in
        Lwt.return(Result.is_ok res)
      | Error err ->
        raise(Failure err)
    )
    >|=
    Alcotest.(check bool)
      "Article can update itself."
      true

  let article_cannot_update_other_article _ () =
    ( let%lwt chris_article' = Article.to_authorizable chris_article in
      match chris_article' with
      | Ok chris_article' ->
        let%lwt res = Article.update_title chris_article' aron_article "Updated Title" in
        Lwt.return(Result.is_error res)
      | Error err ->
        raise(Failure err)
    )
    >|=
    Alcotest.(check bool)
      "Article cannot update another article."
      true

  let set_owner _ () =
    ( let* aron' = User.to_authorizable aron in
      let* chris_article' = Article.update_author aron' chris_article aron in
      let* () =
        if chris_article'.author <> chris
          then Lwt.return_ok()
          else Lwt_result.fail "Article didn't update"
      in
      let* chris' = User.to_authorizable chris in
      let%lwt should_fail = Article.update_title chris' chris_article "Shouldn't work" in
      match should_fail with
      | Ok _ -> Lwt.return_error "Failed to set new owner"
      | Error _ ->
        let* _ = Article.update_author aron' chris_article chris in
        Lwt_result.return true
    )
    >|=
    Alcotest.(check (result bool string))
      "Article cannot update another article."
      (Ok true)

  (** IMPORTANT: the following tests should not compile! *)
  (* let hacker_cannot_update_article () =
    let () = print_endline "about to run a test" in
    Alcotest.(check bool)
      "Article cannot update another article."
      (Result.is_error
        (Article.update_title (Hacker.to_authorizable ben) aron_article "Updated Title"))
      true

    let owner_can_do_nothing () =
    Alcotest.(check bool)
      "authorizable.owner shouldn't be allowed to do anything."
      (Result.is_error
        (Article.update_title (Article.to_authorizable aron_article).owner aron_article "Updated Title"))
      true *)
end

let return =
  let make_test_cases (module Backend : Ocauth.Persistence_s) name =
    let module T = Tests(Backend) in
    [ ( Printf.sprintf "(%s) Managing authorizables." name
      , [ Alcotest_lwt.test_case "Create an authorizable." `Quick T.test_create_authorizable
        ; Alcotest_lwt.test_case "Retrieve an authorizable." `Quick T.test_get_authorizable
        ]
      )
    ; ( Printf.sprintf "(%s) Managing roles." name
      , [ Alcotest_lwt.test_case "Grant a role." `Quick T.test_grant_roles
        ; Alcotest_lwt.test_case "Check roles." `Quick T.test_check_roles
        ]
      )
    ; ( Printf.sprintf "(%s) Managing authorization rules." name
      , [ Alcotest_lwt.test_case "Push rules." `Quick T.test_push_perms
        ; Alcotest_lwt.test_case "Drop rules." `Quick T.test_drop_perms
        ; Alcotest_lwt.test_case "Read rules." `Quick T.test_read_perms
        ]
      )
    ; ( Printf.sprintf "(%s) Admins should be able to do everything." name
      , [ Alcotest_lwt.test_case "Update someone else's article." `Quick T.test_admin_update_others'
        ]
      )
    ; ( Printf.sprintf "(%s) An authorizable should be able to do everything to entities it owns." name
      , [ Alcotest_lwt.test_case "Update own article." `Quick T.test_update_owned
        ]
      )
    ; ( Printf.sprintf "(%s) An authorizable should be able to do everything to itself." name
      , [ Alcotest_lwt.test_case "Update" `Quick T.can_update_self
        ]
      )
    ; ( Printf.sprintf "(%s) Entities should be denied access to entities they shouldn't access." name
      , [ Alcotest_lwt.test_case "Cannot update" `Quick T.cannot_update
        ; Alcotest_lwt.test_case "Cannot update" `Quick T.article_cannot_update_other_article
          (* uncomment the next line to make sure compile-time invariants work *)
          (* ; Alcotest.test_case "Cannot update" `Quick hacker_cannot_update_article *)
        ]
      )
    ; ( Printf.sprintf "(%s) Managing ownership." name
      , [ Alcotest_lwt.test_case "Set owner" `Quick T.set_owner
        ]
      )
    ]
  in
  let test_cases =
    let module MariaConfig =
      struct
        let connection_string = "mariadb://root:my-secret-pw@127.0.0.1:3306/authorization"
      end
    in
    let module Maria =
      (Ocaml_authorize_backends.Mariadb_backend.Make(Role)(MariaConfig)())
    in
    make_test_cases (module Maria) "MariadDB Backend"
    @ make_test_cases (module Ocaml_authorize_backends.Sqlite3_backend.Make(Role)) "SQLite3 Backend"
  in
  let () = Lwt_main.run @@ Alcotest_lwt.run "Authorization" test_cases in
  Ok ()
