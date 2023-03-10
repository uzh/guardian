module Guard = Guardian.Make (Role.Actor) (Role.Target)

module Tests (Backend : Guard.Persistence_s) = struct
  module Article = Article.Make (Backend)
  module Hacker = Hacker.Make (Backend)
  module Post = Post.Make (Backend)
  module UserTarget = User.MakeTarget (Backend)
  module User = User.MakeActor (Backend)
  module Set = Guard.RoleSet

  (* ensure that the `User` module conforms to the `ActorSig` module type and
     `UserTarget` conforms to `TargetSig`. *)
  let _ = (module Article : Guard.TargetSig)
  let _ = (module User : Guard.ActorSig)
  let _ = (module UserTarget : Guard.TargetSig)
  let chris = "Chris", Guardian.Uuid.Actor.create ()
  let aron = "Aron", Guardian.Uuid.Actor.create ()
  let ben : Hacker.t = "Ben Hackerman", Guardian.Uuid.Actor.create ()
  let thomas = "Thomas", Guardian.Uuid.Actor.create ()
  let hugo = "Hugo", Guardian.Uuid.Actor.create ()
  let chris_article = Article.make "Foo" "Bar" chris
  let aron_article = Article.make "Fizz" "Buzz" aron
  let thomas_aron_post = Post.make thomas chris_article "A first reaction"

  let bad_rule =
    ( `Actor (`User, snd chris)
    , Guardian.Action.Update
    , `Target (`Article, aron_article.Article.uuid) )
  ;;

  let global_rules : Guard.Rule.t list =
    let open Guardian.Action in
    [ `ActorEntity `User, Read, `TargetEntity `Article
    ; `ActorEntity `Admin, Manage, `TargetEntity `Article
    ; ( `ActorEntity (`Editor chris_article.Article.uuid)
      , Update
      , `Target (`Article, chris_article.Article.uuid) )
      (* Explanation: Someone with actor rule "Editor of uuid X" has the
         permission to update the "Target with uuid X" *)
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
       let* x = Backend.Target.find_owner ?ctx article.Article.uuid in
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
       Guardian.Uuid.Actor.(to_string chris_art_owner, to_string aron_art_owner))
    >|= Alcotest.(check (result (pair string string) string))
          "Create an authorizable."
          (Ok Guardian.Uuid.Actor.(to_string (snd chris), to_string (snd aron)))
  ;;

  let test_find_authorizable ?ctx _ () =
    (match%lwt Backend.Actor.find_authorizable ?ctx `User (snd aron) with
     | Ok _ -> Lwt.return_true
     | Error err -> failwith err)
    >|= Alcotest.(check bool) "Fetch an authorizable." true
  ;;

  let test_grant_roles ?ctx _ () =
    Backend.Actor.grant_roles ?ctx (snd aron) (Set.singleton `Admin)
    >|= Alcotest.(check (result unit string)) "Grant a role." (Ok ())
  ;;

  let test_check_roles ?ctx _ () =
    (let* roles = Backend.Actor.find_roles ?ctx (snd aron) in
     let expected = Set.of_list [ `User; `Admin ] in
     let diff =
       Set.(union (diff expected roles) (diff roles expected) |> elements)
     in
     if diff = []
     then Lwt.return_ok ()
     else
       let open Set in
       let received = [%show: Role.Actor.t list] (elements roles) in
       let expected = [%show: Role.Actor.t list] (elements expected) in
       Lwt.return_error
         (Format.asprintf
            "Got %s for roles of authorizable %s, but expected %s."
            received
            (Guardian.Uuid.Actor.to_string (snd aron))
            expected))
    >|= Alcotest.(check (result unit string)) "Check a user's roles." (Ok ())
  ;;

  let test_revoke_roles ?ctx _ () =
    (let* () =
       Backend.Actor.grant_roles
         ?ctx
         (snd aron)
         (Set.singleton (`Editor Guardian.Uuid.Target.nil))
     in
     let* () =
       let* roles = Backend.Actor.find_roles ?ctx (snd aron) in
       if Set.mem (`Editor Guardian.Uuid.Target.nil) roles
       then Lwt.return_ok ()
       else
         Lwt.return_error
           "Didn't successfully add the role we intended to remove."
     in
     let* () =
       Backend.Actor.revoke_roles
         ?ctx
         (snd aron)
         (Set.singleton (`Editor Guardian.Uuid.Target.nil))
     in
     let* roles = Backend.Actor.find_roles ?ctx (snd aron) in
     Lwt.return_ok (Set.mem (`Editor Guardian.Uuid.Target.nil) roles))
    >|= Alcotest.(check (result bool string)) "Check a user's roles." (Ok false)
  ;;

  let test_push_rules ?ctx _ () =
    (let* put =
       Backend.Rule.save_all ?ctx global_rules
       |> Lwt_result.map_error [%show: Guard.Rule.t list]
     in
     Lwt.return_ok (CCList.map Guard.Rule.show put))
    >|= Alcotest.(check (result (slist string CCString.compare) string))
          "Push global permissions."
          (Ok (CCList.map Guard.Rule.show global_rules))
  ;;

  let save_existing_rule ?ctx _ () =
    let existing_rule = List.hd global_rules in
    (let* () = Backend.Rule.save ?ctx existing_rule in
     Backend.Rule.save ?ctx existing_rule)
    >|= Alcotest.(check (result unit string)) "Push global permissions." (Ok ())
  ;;

  let test_read_rules ?ctx _ () =
    let open Guard in
    (let%lwt article_rules =
       Backend.Rule.find_all ?ctx (`TargetEntity `Article)
     in
     let%lwt editor_rules =
       Backend.Rule.find_all
         ?ctx
         (`Target (`Article, chris_article.Article.uuid))
     in
     let perms = article_rules @ editor_rules in
     let global_set = Rule.Set.of_list global_rules in
     let retrieved_set = Rule.Set.of_list perms in
     let diff = Rule.Set.diff global_set retrieved_set in
     let diff' = Rule.Set.elements diff |> [%show: Rule.t list] in
     if Rule.Set.compare global_set retrieved_set = 0
     then Lwt.return_ok ()
     else Lwt.return_error (Format.asprintf "Permissions diff: %s." diff'))
    >|= Alcotest.(check (result unit string))
          "Read the global permissions we've just pushed."
          (Ok ())
  ;;

  let test_drop_rules ?ctx _ () =
    (let* () = Backend.Rule.save ?ctx bad_rule in
     let%lwt perms =
       Backend.Rule.find_all
         ?ctx
         (`Target (`Article, aron_article.Article.uuid))
     in
     let* () =
       match perms with
       | [ perm ] when perm = bad_rule -> Lwt.return_ok ()
       | [ _ ] ->
         Lwt.return_error "Failed to push bad permission to test perm dropping."
       | _ -> Lwt.return_error "Invalid permissions."
     in
     let* () = Backend.Rule.delete ?ctx bad_rule in
     let%lwt perms' =
       Backend.Rule.find_all
         ?ctx
         (`Target (`Article, aron_article.Article.uuid))
     in
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

  let owner_can_update ?ctx _ () =
    let open CCFun in
    let%lwt article_owner =
      let open Lwt.Infix in
      Backend.Target.find_owner ?ctx aron_article.Article.uuid
      >|= CCResult.to_opt
          %> CCOption.flatten
          %> CCOption.get_exn_or "Owner not set"
      >>= Backend.Actor.find ?ctx `User
      >|= CCResult.get_or_failwith
    in
    let%lwt try_update =
      Article.update_title ?ctx article_owner aron_article "Updated Title"
    in
    let () =
      Alcotest.(check bool)
        "Actor authorizable of article owner, should be allowed to do anything."
        (CCResult.is_ok try_update)
        true
    in
    Lwt.return_unit
  ;;

  let test_admin_update_others' ?ctx _ () =
    (let%lwt aron_ent = User.to_authorizable ?ctx aron in
     match aron_ent with
     | Ok aron_ent ->
       let%lwt res =
         Article.update_title ?ctx aron_ent chris_article "Updated Title"
       in
       Lwt.return (CCResult.is_ok res)
     | Error err -> failwith err)
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
       Lwt.return (CCResult.is_error res)
     | Error err -> failwith err)
    >|= Alcotest.(check bool) "Chris cannot update an article Aron owns" true
  ;;

  let can_update_self ?ctx _ () =
    (let%lwt (_ : ([> `User ] Backend.target, string) result) =
       UserTarget.to_authorizable ?ctx thomas
     in
     let%lwt thomas_auth = User.to_authorizable ?ctx thomas in
     match thomas_auth with
     | Ok thomas_user ->
       let as_target =
         ( `User
         , Guardian.Uuid.(
             thomas |> snd |> Actor.to_string |> Target.of_string_exn) )
       in
       let%lwt res =
         User.update_name
           ?ctx
           thomas_user
           (fst thomas, as_target)
           "Updated Title"
       in
       Lwt.return (CCResult.is_ok res)
     | Error err -> failwith err)
    >|= Alcotest.(check bool) "Article can update itself." true
  ;;

  let editor_can_edit ?ctx _ () =
    (let* () =
       Backend.Actor.grant_roles
         ?ctx
         (snd thomas)
         (Set.singleton (`Editor chris_article.Article.uuid))
       |> Lwt_result.map_error (fun err -> failwith err)
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
       if chris_article'.Article.author <> chris
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

  let operator_works ?ctx _ () =
    let open Guard in
    let%lwt actual =
      let open Lwt_result.Syntax in
      (* Note: a user can be an actor or a target, so 'to_authorizable' has to
         be called for both roles *)
      let* target' = UserTarget.to_authorizable ?ctx hugo in
      let* actor = User.to_authorizable ?ctx thomas in
      let* () =
        Backend.Actor.grant_roles
          ?ctx
          actor.Actor.uuid
          (RoleSet.singleton (`Editor target'.Target.uuid))
      in
      let* actor = Backend.Actor.find_authorizable ?ctx `User (snd thomas) in
      let* () =
        Backend.Rule.save
          ?ctx
          ( `ActorEntity (`Editor target'.Target.uuid)
          , Guardian.Action.Manage
          , `Target (`User, target'.Target.uuid) )
      in
      let effects =
        EffectSet.One
          (Guardian.Action.Manage, `Target (`User, target'.Target.uuid))
      in
      Backend.validate_effects ?ctx effects actor
    in
    Alcotest.(check (result unit string))
      "Parametric roles work."
      (Ok ())
      actual
    |> Lwt.return
  ;;

  let transistency ?ctx _ () =
    (let* _ = Post.to_authorizable ?ctx thomas_aron_post in
     let* aron_authorizable = User.to_authorizable ?ctx aron in
     let* _thomas_post' =
       Post.update_post
         ?ctx
         aron_authorizable
         thomas_aron_post
         "Update the post comment"
     in
     Lwt.return_ok true)
    >|= Alcotest.(check (result bool string))
          "User can edit an post of an article he's manager of."
          (Ok true)
  ;;

  let transistency_deny ?ctx _ () =
    (let* _ = Post.to_authorizable ?ctx thomas_aron_post in
     let* chris_authorizable = User.to_authorizable ?ctx chris in
     let* _thomas_post' =
       Post.update_post
         ?ctx
         chris_authorizable
         thomas_aron_post
         "Update the post comment"
       |> Lwt_result.map_error (fun err ->
            let check_err =
              CCString.find
                ~sub:"does not satisfy any of the following rules:"
                err
            in
            if check_err |> CCBool.of_int then "correct" else err)
     in
     Lwt.return_ok true)
    >|= Alcotest.(check (result bool string))
          "User cannot edit an post of someone else article."
          (Error "correct")
  ;;

  (** IMPORTANT: the following tests should not compile! *)
  (* let hacker_cannot_update_article ?ctx _ () = let () = print_endline
     "about\n to run a test" in let%lwt ben = Hacker.to_authorizable ?ctx ben |>
     Lwt.map CCResult.get_or_failwith in let%lwt try_update =
     Article.update_title ben aron_article "Updated Title" in let () =
     Alcotest.(check bool) "Article\n cannot update another article."
     (CCResult.is_error try_update) true in Lwt.return_unit ;; *)
end

let () =
  let make_test_cases ?ctx (module Backend : Guard.Persistence_s) name =
    let module T = Tests (Backend) in
    [ ( Format.asprintf "(%s) Managing authorizables." name
      , [ Alcotest_lwt.test_case
            "Create an authorizable."
            `Quick
            (T.test_create_authorizable ?ctx)
        ; Alcotest_lwt.test_case
            "Retrieve an authorizable."
            `Quick
            (T.test_find_authorizable ?ctx)
        ] )
    ; ( Format.asprintf "(%s) Managing roles." name
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
    ; ( Format.asprintf "(%s) Managing authorization rules." name
      , [ Alcotest_lwt.test_case "Push rules." `Quick (T.test_push_rules ?ctx)
        ; Alcotest_lwt.test_case "Drop rules." `Quick (T.test_drop_rules ?ctx)
        ; Alcotest_lwt.test_case "Read rules." `Quick (T.test_read_rules ?ctx)
        ; Alcotest_lwt.test_case "Save rule." `Quick (T.save_existing_rule ?ctx)
        ] )
    ; ( Format.asprintf "(%s) Admins should be able to do everything." name
      , [ Alcotest_lwt.test_case
            "Update someone else's article."
            `Quick
            (T.test_admin_update_others' ?ctx)
        ] )
    ; ( Format.asprintf
          "(%s) An authorizable should be able to do everything to entities it \
           owns."
          name
      , [ Alcotest_lwt.test_case
            "Update own article."
            `Quick
            (T.test_update_owned ?ctx)
        ; Alcotest_lwt.test_case
            "Update own article (via backend)"
            `Quick
            (T.owner_can_update ?ctx)
        ] )
    ; ( Format.asprintf
          "(%s) An authorizable should be able to do everything to itself."
          name
      , [ Alcotest_lwt.test_case "Update" `Quick (T.can_update_self ?ctx) ] )
    ; ( Format.asprintf
          "(%s) Entities should be denied access to entities they shouldn't \
           access."
          name
      , [ Alcotest_lwt.test_case "Cannot update" `Quick (T.cannot_update ?ctx)
          (* uncomment the next line to make sure compile-time invariants
             work *)
          (* ; Alcotest_lwt.test_case "Cannot update" `Quick
             (T.hacker_cannot_update_article ?ctx) *)
        ] )
    ; ( Format.asprintf "(%s) Check access for targeted roles." name
      , [ Alcotest_lwt.test_case
            "Editor can edit"
            `Quick
            (T.editor_can_edit ?ctx)
        ] )
    ; ( Format.asprintf "(%s) Managing ownership." name
      , [ Alcotest_lwt.test_case "Set owner" `Quick (T.set_owner ?ctx) ] )
    ; ( Format.asprintf "(%s) Use parametric roles." name
      , [ Alcotest_lwt.test_case
            "Parametric editor role"
            `Quick
            (T.operator_works ?ctx)
        ] )
    ; ( Format.asprintf
          "(%s) Transistancy, manager of `x` should be able to update `x.a`."
          name
      , [ Alcotest_lwt.test_case "transistency" `Quick (T.transistency ?ctx)
        ; Alcotest_lwt.test_case
            "transistency fail"
            `Quick
            (T.transistency_deny ?ctx)
        ] )
    ]
  in
  let open Guardian_backend.Pools in
  let test_database = "test" in
  let ctx = [ "pool", test_database ] in
  let module MariaConfig = struct
    include DefaultConfig

    let database =
      MultiPools
        [ ( test_database
          , Sys.getenv_opt "DATABASE_URL"
            |> CCOption.get_or ~default:"mariadb://root@database:3306/test" )
        ]
    ;;
  end
  in
  let module Maria =
    Guardian_backend.MariaDb.Make (Role.Actor) (Role.Target)
      (Make (MariaConfig))
  in
  Lwt_main.run
  @@
  let%lwt () = Maria.migrate ~ctx () in
  let%lwt () = Maria.clean ~ctx () in
  make_test_cases ~ctx (module Maria) "MariadDB Backend"
  |> Alcotest_lwt.run "Authorization"
;;
