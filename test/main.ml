module Guardian = Guardian.Make (Role)

module Tests (Backend : Ocauth.Persistence_s) = struct
  module Article = Article.Make (Backend)
  module Hacker = Hacker.Make (Backend)
  module User = User.Make (Backend)

  (* ensure that the `User` module conforms to the `Authorizable_module` module
     type. *)
  let _ = (module User : Guardian.Authorizer.Authorizable_module)
  let _ = (module Article : Guardian.Authorizer.Authorizable_module)
  let chris = "Chris", Uuidm.v `V4
  let aron = "Aron", Uuidm.v `V4
  let ben : Hacker.t = "Ben Hackerman", Uuidm.v `V4
  let thomas = "Thomas", Uuidm.v `V4

  let chris_article =
    Article.make ~title:"Foo" ~content:"Bar" ~uuid:(Uuidm.v `V4) ~author:chris
  ;;

  let aron_article =
    Article.make ~title:"Fizz" ~content:"Buzz" ~uuid:(Uuidm.v `V4) ~author:aron
  ;;

  let bad_rule = `One (snd chris), `Update, `One aron_article.uuid

  let global_rules : Guardian.Authorizer.auth_rule list =
    [ `Entity `User, `Read, `Entity `Article
    ; `Entity `Admin, `Manage, `Entity `Article
    ; `Entity (`Editor chris_article.uuid), `Update, `One chris_article.uuid
    ]
  ;;

  let ( let* ) = Lwt_result.bind
  let ( >|= ) = Lwt.Infix.( >|= )

  let test_create_authorizable ?ctx _ () =
    (let* _aron_ent = User.to_authorizable ?ctx aron in
     let* _chris_ent = User.to_authorizable ?ctx chris in
     let* _ben_ent = Hacker.to_authorizable ?ctx ben in
     let* _tomas_ent = User.to_authorizable ?ctx thomas in
     let* _chris_art_ent = Article.to_authorizable ?ctx chris_article in
     let* _aron_art_ent = Article.to_authorizable ?ctx aron_article in
     (* now we check to see that the authorizables have had ownership set *)
     let find_owner_id article =
       let* x = Backend.find_owner ?ctx article.Article.uuid in
       x
       |> CCOption.to_result
            (Format.asprintf
               "Couldn't get owner for article %s"
               (Article.show article))
       |> Lwt.return
     in
     let* chris_art_owner = find_owner_id chris_article in
     let* aron_art_owner = find_owner_id aron_article in
     Lwt.return_ok
       (Uuidm.to_string chris_art_owner, Uuidm.to_string aron_art_owner))
    >|= Alcotest.(check (result (pair string string) string))
          "Create an authorizable."
          (Ok (Uuidm.to_string (snd chris), Uuidm.to_string (snd aron)))
  ;;

  let test_find_authorizable ?ctx _ () =
    (match%lwt Backend.find_authorizable ?ctx ~typ:`User (snd aron) with
     | Ok _ -> Lwt.return_true
     | Error err -> raise (Failure err))
    >|= Alcotest.(check bool) "Fetch an authorizable." true
  ;;

  let test_grant_roles ?ctx _ () =
    Backend.grant_roles ?ctx (snd aron) (Guardian.Role_set.singleton `Admin)
    >|= Alcotest.(check (result unit string)) "Grant a role." (Ok ())
  ;;

  let test_check_roles ?ctx _ () =
    (let* roles = Backend.find_roles ?ctx (snd aron) in
     let expected = Guardian.Role_set.of_list [ `User; `Admin ] in
     let diff =
       Guardian.Role_set.(
         union (diff expected roles) (diff roles expected) |> elements)
     in
     if diff = []
     then Lwt.return_ok ()
     else
       let open Ocauth.Role_set in
       let received = [%show: Role.t list] (elements roles) in
       let expected = [%show: Role.t list] (elements expected) in
       Lwt.return_error
         (Printf.sprintf
            "Got %s for roles of authorizable %s, but expected %s."
            received
            (Uuidm.to_string (snd aron))
            expected))
    >|= Alcotest.(check (result unit string)) "Check a user's roles." (Ok ())
  ;;

  let test_revoke_roles ?ctx _ () =
    (let* () =
       Backend.grant_roles
         ?ctx
         (snd aron)
         (Ocauth.Role_set.singleton (`Editor Uuidm.nil))
     in
     let* () =
       let* roles = Backend.find_roles ?ctx (snd aron) in
       if Ocauth.Role_set.mem (`Editor Uuidm.nil) roles
       then Lwt.return_ok ()
       else
         Lwt.return_error
           "Didn't successfully add the role we intended to remove."
     in
     let* () =
       Backend.revoke_roles
         ?ctx
         (snd aron)
         (Ocauth.Role_set.singleton (`Editor Uuidm.nil))
     in
     let* roles = Backend.find_roles ?ctx (snd aron) in
     Lwt.return_ok (Ocauth.Role_set.mem (`Editor Uuidm.nil) roles))
    >|= Alcotest.(check (result bool string)) "Check a user's roles." (Ok false)
  ;;

  let test_push_rules ?ctx _ () =
    (let* put =
       Backend.save_rules ?ctx global_rules
       |> Lwt_result.map_error [%show: Ocauth.Authorizer.auth_rule list]
     in
     Lwt.return_ok (CCList.map Ocauth.Authorizer.show_auth_rule put))
    >|= Alcotest.(check (result (slist string String.compare) string))
          "Push global permissions."
          (Ok (CCList.map Ocauth.Authorizer.show_auth_rule global_rules))
  ;;

  let test_read_rules ?ctx _ () =
    (let* article_rules = Backend.find_rules ?ctx (`Entity `Article) in
     let* editor_rules = Backend.find_rules ?ctx (`One chris_article.uuid) in
     let perms = article_rules @ editor_rules in
     let global_set = Guardian.Authorizer.Auth_rule_set.of_list global_rules in
     let retrieved_set = Guardian.Authorizer.Auth_rule_set.of_list perms in
     let diff =
       Guardian.Authorizer.Auth_rule_set.diff global_set retrieved_set
     in
     let diff' =
       Guardian.Authorizer.Auth_rule_set.elements diff
       |> [%show: Guardian.Authorizer.auth_rule list]
     in
     if Guardian.Authorizer.Auth_rule_set.compare global_set retrieved_set = 0
     then Lwt.return_ok ()
     else Lwt.return_error (Printf.sprintf "Permissions diff: %s." diff'))
    >|= Alcotest.(check (result unit string))
          "Read the global permissions we've just pushed."
          (Ok ())
  ;;

  let test_drop_rules ?ctx _ () =
    (let* () = Backend.save_rule ?ctx bad_rule in
     let* perms = Backend.find_rules ?ctx (`One aron_article.uuid) in
     let* () =
       match perms with
       | [ perm ] ->
         if perm = bad_rule
         then Lwt.return_ok ()
         else
           Lwt.return_error
             "Failed to push bad permission to test perm dropping."
       | _ -> Lwt.return_error "Invalid permissions."
     in
     let* () = Backend.delete_rule ?ctx bad_rule in
     let* perms' = Backend.find_rules ?ctx (`One aron_article.uuid) in
     match perms' with
     | [] -> Lwt.return_ok ()
     | _ -> Lwt.return_error "Failed to remove bad perm.")
    >|= Alcotest.(check (result unit string))
          "Read the global permissions we've just pushed."
          (Ok ())
  ;;

  let test_update_owned ?ctx _ () =
    (let* chris_ent = User.to_authorizable ?ctx chris in
     let* _art =
       Article.update_title ?ctx chris_ent chris_article "Updated Title"
     in
     Lwt.return_ok true)
    >|= Alcotest.(check (result bool string))
          "Chris can update an article owned by Chris."
          (Ok true)
  ;;

  let test_admin_update_others' ?ctx _ () =
    (let%lwt aron_ent = User.to_authorizable ?ctx aron in
     match aron_ent with
     | Ok aron_ent ->
       let%lwt res =
         Article.update_title ?ctx aron_ent chris_article "Updated Title"
       in
       Lwt.return (Result.is_ok res)
     | Error err -> raise (Failure err))
    >|= Alcotest.(check bool)
          "Aron (admin) can update an article Chris owns"
          true
  ;;

  let cannot_update ?ctx _ () =
    (let%lwt chris_ent = User.to_authorizable ?ctx chris in
     match chris_ent with
     | Ok chris_ent ->
       let%lwt res =
         Article.update_title ?ctx chris_ent aron_article "Updated Title"
       in
       Lwt.return (Result.is_error res)
     | Error err -> raise (Failure err))
    >|= Alcotest.(check bool) "Chris cannot update an article Aron owns" true
  ;;

  let can_update_self ?ctx _ () =
    (let%lwt chris_article' = Article.to_authorizable ?ctx chris_article in
     match chris_article' with
     | Ok chris_article' ->
       let%lwt res =
         Article.update_title ?ctx chris_article' chris_article "Updated Title"
       in
       Lwt.return (Result.is_ok res)
     | Error err -> raise (Failure err))
    >|= Alcotest.(check bool) "Article can update itself." true
  ;;

  let article_cannot_update_other_article ?ctx _ () =
    (let%lwt chris_article' = Article.to_authorizable ?ctx chris_article in
     match chris_article' with
     | Ok chris_article' ->
       let%lwt res =
         Article.update_title ?ctx chris_article' aron_article "Updated Title"
       in
       Lwt.return (Result.is_error res)
     | Error err -> raise (Failure err))
    >|= Alcotest.(check bool) "Article cannot update another article." true
  ;;

  let editor_can_edit ?ctx _ () =
    (let* () =
       Backend.grant_roles
         ?ctx
         (snd thomas)
         (Ocauth.Role_set.singleton (`Editor chris_article.uuid))
     in
     let* thomas_authorizable = User.to_authorizable ?ctx thomas in
     let* _chris_article' =
       Article.update_title
         ?ctx
         thomas_authorizable
         chris_article
         "Thomas set this one"
     in
     Lwt.return_ok true)
    >|= Alcotest.(check (result bool string))
          "Editor can edit an article he's assigned as the editor of."
          (Ok true)
  ;;

  let set_owner ?ctx _ () =
    (let* aron' = User.to_authorizable ?ctx aron in
     let* chris_article' =
       Article.update_author ?ctx aron' chris_article aron
     in
     let* () =
       if chris_article'.author <> chris
       then Lwt.return_ok ()
       else Lwt_result.fail "Article didn't update"
     in
     let* chris' = User.to_authorizable ?ctx chris in
     let%lwt should_fail =
       Article.update_title ?ctx chris' chris_article "Shouldn't work"
     in
     match should_fail with
     | Ok _ -> Lwt.return_error "Failed to set new owner"
     | Error _ ->
       let* _ = Article.update_author ?ctx aron' chris_article chris in
       Lwt_result.return true)
    >|= Alcotest.(check (result bool string))
          "Article cannot update another article."
          (Ok true)
  ;;

  (** IMPORTANT: the following tests should not compile! *)
  (* let hacker_cannot_update_article () = let () = print_endline "about to run
     a test" in Alcotest.(check bool) "Article cannot update another article."
     (Result.is_error (Article.update_title (Hacker.to_authorizable ben)
     aron_article "Updated Title")) true

     let owner_can_do_nothing () = Alcotest.(check bool) "authorizable.owner
     shouldn't be allowed to do anything." (Result.is_error
     (Article.update_title (Article.to_authorizable aron_article).owner
     aron_article "Updated Title")) true *)
end

let () =
  let make_test_cases ?ctx (module Backend : Ocauth.Persistence_s) name =
    let module T = Tests (Backend) in
    [ ( Printf.sprintf "(%s) Managing authorizables." name
      , [ Alcotest_lwt.test_case
            "Create an authorizable."
            `Quick
            (T.test_create_authorizable ?ctx)
        ; Alcotest_lwt.test_case
            "Retrieve an authorizable."
            `Quick
            (T.test_find_authorizable ?ctx)
        ] )
    ; ( Printf.sprintf "(%s) Managing roles." name
      , [ Alcotest_lwt.test_case
            "Grant a role."
            `Quick
            (T.test_grant_roles ?ctx)
        ; Alcotest_lwt.test_case "Check roles." `Quick (T.test_check_roles ?ctx)
        ; Alcotest_lwt.test_case
            "Revoke a role."
            `Quick
            (T.test_revoke_roles ?ctx)
        ] )
    ; ( Printf.sprintf "(%s) Managing authorization rules." name
      , [ Alcotest_lwt.test_case "Push rules." `Quick (T.test_push_rules ?ctx)
        ; Alcotest_lwt.test_case "Drop rules." `Quick (T.test_drop_rules ?ctx)
        ; Alcotest_lwt.test_case "Read rules." `Quick (T.test_read_rules ?ctx)
        ] )
    ; ( Printf.sprintf "(%s) Admins should be able to do everything." name
      , [ Alcotest_lwt.test_case
            "Update someone else's article."
            `Quick
            (T.test_admin_update_others' ?ctx)
        ] )
    ; ( Printf.sprintf
          "(%s) An authorizable should be able to do everything to entities it \
           owns."
          name
      , [ Alcotest_lwt.test_case
            "Update own article."
            `Quick
            (T.test_update_owned ?ctx)
        ] )
    ; ( Printf.sprintf
          "(%s) An authorizable should be able to do everything to itself."
          name
      , [ Alcotest_lwt.test_case "Update" `Quick (T.can_update_self ?ctx) ] )
    ; ( Printf.sprintf
          "(%s) Entities should be denied access to entities they shouldn't \
           access."
          name
      , [ Alcotest_lwt.test_case "Cannot update" `Quick (T.cannot_update ?ctx)
        ; Alcotest_lwt.test_case
            "Cannot update"
            `Quick
            (T.article_cannot_update_other_article ?ctx)
          (* uncomment the next line to make sure compile-time invariants work *)
          (* ; Alcotest.test_case "Cannot update" `Quick hacker_cannot_update_article *)
        ] )
    ; ( Printf.sprintf "(%s) Check access for targeted roles." name
      , [ Alcotest_lwt.test_case
            "Editor can edit"
            `Quick
            (T.editor_can_edit ?ctx)
        ] )
    ; ( Printf.sprintf "(%s) Managing ownership." name
      , [ Alcotest_lwt.test_case "Set owner" `Quick (T.set_owner ?ctx) ] )
    ]
  in
  let open Guardian_backend.Pools in
  let test_database = "test" in
  let module MariaConfig = struct
    include DefaultConfig

    let database =
      MultiPools
        [ "test", "mariadb://root@database:3306/test"
        ; test_database, "mariadb://root@database:3306/test"
        ]
    ;;
  end
  in
  let module Maria = Guardian_backend.MariaDb.Make (Role) (Make (MariaConfig))
  in
  let module Sqlite = Guardian_backend.Sqlite.Make (Role) in
  Lwt_main.run
  @@ let%lwt () = Maria.migrate ~ctx:[ "pool", test_database ] () in
     let%lwt () = Sqlite.migrate () in
     make_test_cases
       ~ctx:[ "pool", test_database ]
       (module Maria)
       "MariadDB Backend"
     @ make_test_cases (module Sqlite) "SQLite3 Backend"
     |> Alcotest_lwt.run "Authorization"
;;
