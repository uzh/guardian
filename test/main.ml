module Guard = Guardian.Make (Role.Actor) (Role.Role) (Role.Target)

let aron_article_id = Guard.Uuid.Target.create ()
let chris_article_id = Guard.Uuid.Target.create ()
let thomas_chris_post_id = Guard.Uuid.Target.create ()

module Tests (Backend : Guard.PersistenceSig) = struct
  open Guard
  open Permission
  module Article = Article.Make (Backend)
  module Hacker = Hacker.Make (Backend)
  module Notes = Notes.Make (Backend)
  module Post = Post.Make (Backend)
  module UserTarget = User.MakeTarget (Backend)
  module User = User.MakeActor (Backend)
  module ActorRoleSet = CCSet.Make (Guard.ActorRole)
  module ActorPermissionSet = CCSet.Make (Guard.ActorPermission)
  module RolePermissionSet = CCSet.Make (Guard.RolePermission)

  let testable_uuid = Uuid.Target.(Alcotest.testable pp equal)
  let testable_actor_uuid = Uuid.Actor.(Alcotest.testable pp equal)
  let testable_actor_role = ActorRole.(Alcotest.testable pp equal)
  let testable_set = Set.(Alcotest.testable pp equal)

  let testable_role_permission_set =
    RolePermissionSet.(Alcotest.testable (pp RolePermission.pp) equal)
  ;;

  (* ensure that the `User` module conforms to the `ActorSig` and `UserTarget`
     conforms to `TargetSig` module type. *)
  let _ = (module Article : TargetSig)
  let _ = (module User : ActorSig)
  let _ = (module UserTarget : TargetSig)
  let chris = "Chris", Guard.Uuid.Actor.create ()
  let aron = "Aron", Guard.Uuid.Actor.create ()
  let ben : Hacker.t = "Ben Hackerman", Guard.Uuid.Actor.create ()
  let thomas = "Thomas", Guard.Uuid.Actor.create ()
  let hugo = "Hugo", Guard.Uuid.Actor.create ()
  let chris_article = Article.make ~id:chris_article_id "Foo" "Bar" chris
  let aron_article = Article.make ~id:aron_article_id "Fizz" "Buzz" aron

  let thomas_chris_post =
    Post.make ~id:thomas_chris_post_id thomas chris_article "A first reaction"
  ;;

  let thomas_aron_post = Post.make thomas aron_article "A second reaction"
  let thomas_note = Notes.make thomas "Hello world note"
  let chris_note = Notes.make chris "My private note"
  let bad_role_permission = RolePermission.create `Editor Update `Article

  let bad_actor_permission =
    ActorPermission.create_for_id (snd chris) Update aron_article.Article.id
  ;;

  let global_role_permission =
    [ `Reader, Read, `Article
    ; `Editor, Create, `Article
    ; `Editor, Update, `Article
    ]
    |> CCList.map (fun (role, perm, model) ->
           RolePermission.create role perm model)
  ;;

  let ( let* ) = Lwt_result.bind
  let ( >|= ) = Lwt.Infix.( >|= )

  let test_create_authorizable ?ctx (_ : 'a) () =
    let expected =
      ( ActorRole.create ~target_uuid:chris_article_id (snd chris) `Author
      , ActorRole.create ~target_uuid:aron_article_id (snd aron) `Author )
    in
    (let* _aron_ent = User.to_authorizable ?ctx aron in
     let* _chris_ent = User.to_authorizable ?ctx chris in
     let* _ben_ent = Hacker.to_authorizable ?ctx ben in
     let* _tomas_ent = User.to_authorizable ?ctx thomas in
     let* _chris_art_ent = Article.to_authorizable ?ctx chris_article in
     let* _aron_art_ent = Article.to_authorizable ?ctx aron_article in
     (* now we check to see that the authorizables have had ownership set *)
     let find_author_role ({ Article.id; _ } as article) =
       Backend.ActorRole.find_by_target ?ctx (`Author, id)
       >|= function
       | [] ->
         Error
           (Article.show article
           |> Format.asprintf "Couldn't get owner for article %s")
       | roles -> Ok (CCList.hd roles)
     in
     let* chris_art_owner = find_author_role chris_article in
     let* aron_art_owner = find_author_role aron_article in
     Lwt.return_ok (chris_art_owner, aron_art_owner))
    >|= Alcotest.(
          check (result (pair testable_actor_role testable_actor_role) string))
          "Create an authorizable."
          (Ok expected)
  ;;

  let test_find_authorizable ?ctx (_ : 'a) () =
    (match%lwt Backend.Actor.find ?ctx (snd aron) with
    | Ok _ -> Lwt.return_true
    | Error err -> failwith err)
    >|= Alcotest.(check bool) "Fetch an authorizable." true
  ;;

  let test_grant_roles ?ctx (_ : 'a) () =
    let role = ActorRole.create (snd aron) in
    let%lwt () =
      ActorRole.create (snd aron) `Admin
      |> Backend.ActorRole.upsert ?ctx
      >|= Alcotest.(check unit) "Grant a role." ()
    in
    Backend.ActorRole.find_by_actor ?ctx (snd aron)
    >|= Alcotest.(check (list testable_actor_role))
          "Check if the role was granted."
          [ role `Admin; role `Reader (* every user gets `Reader role *) ]
  ;;

  let test_check_roles ?ctx (_ : 'a) () =
    let open ActorRoleSet in
    (let%lwt roles =
       Backend.ActorRole.find_by_actor ?ctx (snd aron) >|= of_list
     in
     let expected =
       let role = ActorRole.create (snd aron) in
       [ role `Admin; role `Reader (* every user gets `Reader role *) ]
       |> of_list
     in
     if compare (diff expected roles) (diff roles expected) = 0
     then Lwt.return_ok ()
     else (
       let received = [%show: ActorRole.t list] (elements roles) in
       let expected = [%show: ActorRole.t list] (elements expected) in
       Lwt.return_error
         (Format.asprintf
            "Got %s for roles of authorizable %s, but expected %s."
            received
            (Guard.Uuid.Actor.to_string (snd aron))
            expected)))
    >|= Alcotest.(check (result unit string)) "Check a user's roles." (Ok ())
  ;;

  let test_revoke_roles ?ctx (_ : 'a) () =
    (let open ActorRoleSet in
    let role = ActorRole.create (snd aron) `Editor in
    let open Backend in
    let find_role () = ActorRole.find_by_actor ?ctx (snd aron) >|= of_list in
    let%lwt () = role |> ActorRole.upsert ?ctx in
    let* () =
      let%lwt roles = find_role () in
      if mem role roles
      then Lwt.return_ok ()
      else
        Lwt.return_error
          "Didn't successfully add the role we intended to remove."
    in
    let%lwt () = role |> ActorRole.delete ?ctx in
    let%lwt roles = find_role () in
    Lwt.return_ok (mem role roles))
    >|= Alcotest.(check (result bool string)) "Check a user's roles." (Ok false)
  ;;

  let test_push_role_permission ?ctx (_ : 'a) () =
    (* let open RolePermissionSet in *)
    let testable_set = testable_role_permission_set in
    Backend.RolePermission.insert_all ?ctx global_role_permission
    |> Lwt_result.map_error RolePermissionSet.of_list
    |> Lwt_result.map RolePermissionSet.of_list
    >|= Alcotest.(check (result testable_set testable_set))
          "Push global permissions."
          (Ok (global_role_permission |> RolePermissionSet.of_list))
  ;;

  let save_existing_rule ?ctx (_ : 'a) () =
    let existing_rule = List.hd global_role_permission in
    (let* () = Backend.RolePermission.insert ?ctx existing_rule in
     Backend.RolePermission.insert ?ctx existing_rule)
    >|= Alcotest.(check (result unit string)) "Save existing permission" (Ok ())
  ;;

  let test_read_rules ?ctx (_ : 'a) () =
    let open ActorPermissionSet in
    (let actor_permissions =
       TargetEntity.
         [ Model `Article
         ; Id chris_article.Article.id
         ; Id chris_note.Notes.id
         ; Id thomas_note.Notes.id
         ]
     in
     let%lwt perms =
       Lwt_list.map_s
         (Backend.ActorPermission.find_all_of_entity ?ctx)
         actor_permissions
       >|= CCList.flatten
     in
     let expexted_set = of_list [] in
     let retrieved_set = of_list perms in
     let diff = diff expexted_set retrieved_set in
     if compare expexted_set retrieved_set = 0
     then Lwt.return_ok ()
     else
       Lwt.return_error
         (elements diff
         |> [%show: RolePermission.t list]
         |> Format.asprintf "Permissions diff: %s."))
    >|= Alcotest.(check (result unit string))
          "Read the global permissions we've just pushed."
          (Ok ())
  ;;

  (* let test_drop_rules ?ctx (_ : 'a) () = (let open Backend.RolePermission in
     let open RolePermissionSet in let* () = insert ?ctx bad_role_permission in
     let* (_ : t) = Backend.RolePermission.find_all_of_model ?ctx `Article >|=
     of_list >|= fun perms -> if mem bad_role_permission perms then Ok perms
     else Error "Failed to push bad permission to test perm dropping." in let*
     () = Backend.RolePermission.delete ?ctx bad_role_permission in
     Backend.RolePermission.find_all_of_model ?ctx `Article >|= of_list >|= fun
     perms -> if mem bad_role_permission perms |> not then Ok () else Error
     "Failed to remove bad permission.") >|= Alcotest.(check (result unit
     string)) "Read the global permissions we've just pushed." (Ok ()) ;; *)

  let test_update_owned ?ctx (_ : 'a) () =
    (let* chris_ent = User.to_authorizable ?ctx chris in
     let* (_ : Article.t) =
       Article.update_title ?ctx chris_ent chris_article "Updated Title"
     in
     Lwt.return_ok true)
    >|= Alcotest.(check (result bool string))
          "Chris can update an article owned by Chris."
          (Ok true)
  ;;

  let test_admin_update_others' ?ctx (_ : 'a) () =
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

  let cannot_update ?ctx (_ : 'a) () =
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

  let can_update_self ?ctx (_ : 'a) () =
    (let%lwt (_ : (Backend.target, string) result) =
       UserTarget.to_authorizable ?ctx thomas
     in
     let%lwt thomas_auth = User.to_authorizable ?ctx thomas in
     match thomas_auth with
     | Ok thomas_user ->
       let as_target =
         Guard.Uuid.(thomas |> snd |> Actor.to_string |> Target.of_string_exn)
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

  let editor_cannot_read_other_article ?ctx (_ : 'a) () =
    let open Alcotest in
    let open ValidationSet in
    let pp_error set =
      Format.asprintf "Permission denied for %s" ([%show: ValidationSet.t] set)
    in
    (let* _ = Notes.to_authorizable ?ctx thomas_note in
     let* _ = Notes.to_authorizable ?ctx chris_note in
     let%lwt () =
       ActorRole.create ~target_uuid:thomas_note.Notes.id (snd thomas) `Reader
       |> Backend.ActorRole.upsert ?ctx
     in
     let* thomas_actor = User.to_authorizable ?ctx thomas in
     let validate_test (fcn : string -> string) msg set expected =
       Backend.validate ?ctx CCFun.id set thomas_actor
       |> Lwt_result.map_error fcn
       |> Lwt.map (check (result unit string) msg expected)
     in
     let%lwt () =
       validate_test
         CCFun.id
         "Reader can read a note he's reader/owner of."
         (One (Permission.Read, TargetEntity.Id thomas_note.Notes.id))
         (Ok ())
     in
     let%lwt () =
       let set = One (Permission.Read, TargetEntity.Model `Note) in
       validate_test
         (fun _ -> pp_error set)
         "Reader cannot read a note of anyone."
         set
         (Error (pp_error set))
     in
     let%lwt () =
       let set = One (Permission.Read, TargetEntity.Id chris_note.Notes.id) in
       validate_test
         (fun _ -> pp_error set)
         "Reader cannot read a note of someone else."
         set
         (Error (pp_error set))
     in
     Lwt.return_ok true)
    >|= Alcotest.(check (result bool string))
          "Reader cannot read a note of any-/someone else."
          (Ok true)
  ;;

  let editor_can_edit ?ctx (_ : 'a) () =
    (let%lwt () =
       ActorRole.create
         ~target_uuid:chris_article.Article.id
         (snd thomas)
         `Editor
       |> Backend.ActorRole.upsert ?ctx
     in
     let* thomas_actor = User.to_authorizable ?ctx thomas in
     let* _chris_article' =
       Article.update_title
         ?ctx
         thomas_actor
         chris_article
         "Thomas set this one"
     in
     Lwt.return_ok true)
    >|= Alcotest.(check (result bool string))
          "Editor can edit an article he's assigned as the editor of."
          (Ok true)
  ;;

  let set_owner ?ctx (_ : 'a) () =
    (let* aron' = User.to_authorizable ?ctx aron in
     let* chris_article' =
       Article.update_author ?ctx aron' chris_article chris aron
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
       let* (_ : Article.t) =
         Article.update_author ?ctx aron' chris_article aron chris
       in
       Lwt_result.return true)
    >|= Alcotest.(check (result bool string))
          "Article cannot update another article."
          (Ok true)
  ;;

  let operator_works ?ctx (_ : 'a) () =
    let open Guard in
    let%lwt actual =
      let open Lwt_result.Syntax in
      (* Note: a user can be an actor or a target, so 'to_authorizable' has to
         be called for both roles *)
      let* target' = UserTarget.to_authorizable ?ctx hugo in
      let target_uuid = target'.Target.uuid in
      let* actor = User.to_authorizable ?ctx aron in
      let%lwt () =
        ActorRole.create ~target_uuid actor.Actor.uuid `Editor
        |> Backend.ActorRole.upsert ?ctx
      in
      let* actor = Backend.Actor.find ?ctx (snd aron) in
      let* () =
        RolePermission.create `Admin Permission.Manage `User
        |> Backend.RolePermission.insert ?ctx
      in
      let effects =
        ValidationSet.One (Permission.Manage, TargetEntity.Id target_uuid)
      in
      Backend.validate ?ctx CCFun.id effects actor
    in
    Alcotest.(check (result unit string))
      "Parametric roles work."
      (Ok ())
      actual
    |> Lwt.return
  ;;

  let transistency ?ctx (_ : 'a) () =
    (let* (_ : Guard.Target.t) = Post.to_authorizable ?ctx thomas_chris_post in
     let* chris_authorizable = User.to_authorizable ?ctx chris in
     let* _thomas_post' =
       Post.update_post
         ?ctx
         chris_authorizable
         thomas_chris_post
         "Update the post comment"
     in
     Lwt.return_ok true)
    >|= Alcotest.(check (result bool string))
          "User can edit an post of an article he's manager of."
          (Ok true)
  ;;

  let transistency_deny ?ctx (_ : 'a) () =
    (let* (_ : Guard.Target.t) = Post.to_authorizable ?ctx thomas_aron_post in
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

  let roles_exist_for_type ?ctx (_ : 'a) () =
    let result =
      let validation_set = ValidationSet.One (Read, TargetEntity.Model `Post) in
      let* actor = User.to_authorizable ?ctx chris in
      Backend.validate ?ctx CCFun.id validation_set actor
    in
    result
    >|= Alcotest.(check (result unit string))
          "Post should be readable by user."
          (Ok ())
  ;;

  let test_find_by_role ?ctx (_ : 'a) () =
    let sort = CCList.stable_sort Guard.Uuid.Actor.compare in
    let%lwt roles =
      Backend.ActorRole.find_actors_by_role
        ?ctx
        ~exclude:[ `Editor, None ]
        (`Reader, None)
    in
    let expect = [ snd aron; snd chris; snd thomas ] in
    Alcotest.(check (list testable_actor_uuid))
      "return correct list of actor uuid by role `User."
      (expect |> sort)
      (roles |> sort)
    |> Lwt.return
  ;;

  (* let test_find_by_roles ?ctx (_ : 'a) () = let sort = CCList.stable_sort
     Guard.Uuid.Actor.compare in let%lwt roles =
     Backend.ActorRole.find_actors_by_roles ?ctx RoleEntity.[ Model `Admin;
     Model `Editor ] |> Lwt.map CCFun.(CCList.(map snd %> flatten)) in let
     expect = [ snd aron; snd ben ] in Alcotest.(check (list
     testable_actor_uuid)) "return correct list of actor uuid by roles." (expect
     |> sort) (roles |> CCList.map (fun actor -> actor.Actor.uuid) |> sort) |>
     Lwt.return ;; *)

  (** IMPORTANT: the following tests should not compile! *)
  (* let hacker_cannot_update_article ?ctx (_:'a) () = let () = print_endline
     "about\n to run a test" in let%lwt ben = Hacker.to_authorizable ?ctx ben |>
     Lwt.map CCResult.get_or_failwith in let%lwt try_update =
     Article.update_title ben aron_article "Updated Title" in let () =
     Alcotest.(check bool) "Article\n cannot update another article."
     (CCResult.is_error try_update) true in Lwt.return_unit ;; *)
end

let () =
  let make_test_cases ?ctx (module Backend : Guard.PersistenceSig) name =
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
      , [ Alcotest_lwt.test_case
            "Push rules."
            `Quick
            (T.test_push_role_permission ?ctx)
          (* ; Alcotest_lwt.test_case "Drop rules." `Quick (T.test_drop_rules
             ?ctx) *)
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
        ; Alcotest_lwt.test_case
            "Cannot read"
            `Quick
            (T.editor_cannot_read_other_article ?ctx)
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
    ; ( Format.asprintf "(%s) Validate returned 'exists' sql for kinds." name
      , [ Alcotest_lwt.test_case
            "transistency"
            `Quick
            (T.roles_exist_for_type ?ctx)
        ] )
    ; ( Format.asprintf "(%s) Find all actors of a specific role" name
      , [ Alcotest_lwt.test_case "role" `Quick (T.test_find_by_role ?ctx) ] )
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
    Guardian_backend.MariaDb.Make (Role.Actor) (Role.Role) (Role.Target)
      (Make (MariaConfig))
  in
  Lwt_main.run
  @@ let%lwt () = Maria.delete ~ctx () in
     let%lwt () = Maria.migrate ~ctx () in
     let%lwt () = Maria.clean ~ctx () in
     let%lwt () = Maria.start ~ctx () in
     make_test_cases ~ctx (module Maria) "MariadDB Backend"
     |> Alcotest_lwt.run "Authorization"
;;
