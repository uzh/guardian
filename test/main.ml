open CCFun
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

  let testable_actor_role_set =
    ActorRoleSet.(Alcotest.testable (pp ActorRole.pp) equal)
  ;;

  let testable_role_permission_set =
    RolePermissionSet.(Alcotest.testable (pp RolePermission.pp) equal)
  ;;

  let testable_permission_on_target =
    PermissionOnTarget.(Alcotest.testable pp equal)
  ;;

  let testable_role_assignment = RoleAssignment.(Alcotest.testable pp equal)

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
  let bad_role_permission = RolePermission.create `Editor Update `Note

  let bad_actor_permission =
    ActorPermission.create_for_id (snd chris) Update aron_article.Article.id
  ;;

  let global_role_permission =
    [ `Reader, Read, `Article
    ; `Reader, Read, `Post
    ; `Editor, Create, `Article
    ; `Editor, Update, `Article
    ; `Admin, Manage, `Article
    ; `Admin, Manage, `Post
    ; `Admin, Manage, `Note
    ; `Author, Update, `Article
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
    (let* aron_ent = User.to_authorizable ?ctx aron in
     let* () = User.insert_roles ?ctx aron_ent in
     let* chris_ent = User.to_authorizable ?ctx chris in
     let* () = User.insert_roles ?ctx chris_ent in
     let* _ben_ent = Hacker.to_authorizable ?ctx ben in
     let* thomas_ent = User.to_authorizable ?ctx thomas in
     let* () = User.insert_roles ?ctx thomas_ent in
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
    let open ActorRoleSet in
    let%lwt previous_roles =
      Backend.ActorRole.find_by_actor ?ctx (snd aron) >|= of_list
    in
    let grant_role = ActorRole.create (snd aron) `Admin in
    let%lwt () =
      Backend.ActorRole.upsert ?ctx grant_role
      >|= Alcotest.(check unit) "Grant a role." ()
    in
    Backend.ActorRole.find_by_actor ?ctx (snd aron)
    >|= of_list
    >|= CCFun.flip diff previous_roles
    >|= Alcotest.(check testable_actor_role_set)
          "Check if the role was granted."
          (ActorRoleSet.singleton grant_role)
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
    let open RolePermissionSet in
    let testable_set = testable_role_permission_set in
    Backend.RolePermission.insert_all ?ctx global_role_permission
    |> Lwt_result.map_error of_list
    |> Lwt_result.map of_list
    >|= Alcotest.(check (result testable_set testable_set))
          "Push global permissions."
          (Ok (global_role_permission |> of_list))
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
          |> [%show: ActorPermission.t list]
          |> Format.asprintf "Permissions diff: %s."))
    >|= Alcotest.(check (result unit string))
          "Read the global permissions we've just pushed."
          (Ok ())
  ;;

  let test_drop_rules ?ctx (_ : 'a) () =
    (let open Backend.RolePermission in
     let open RolePermissionSet in
     let* () = insert ?ctx bad_role_permission in
     let* (_ : t) =
       find_all_of_model ?ctx `Note
       >|= of_list
       >|= fun perms ->
       if mem bad_role_permission perms
       then Ok perms
       else Error "Failed to push bad permission to test perm dropping."
     in
     let* () = delete ?ctx bad_role_permission in
     find_all_of_model ?ctx `Article
     >|= of_list
     >|= fun perms ->
     if mem bad_role_permission perms |> not
     then Ok ()
     else Error "Failed to remove bad permission.")
    >|= Alcotest.(check (result unit string))
          "Read the global permissions we've just pushed."
          (Ok ())
  ;;

  let test_update_by_author ?ctx (_ : 'a) () =
    (let* chris_ent = User.to_authorizable ?ctx chris in
     let* (_ : Article.t) =
       Article.update_title ?ctx chris_ent chris_article "Updated Title"
     in
     Lwt.return_ok true)
    >|= Alcotest.(check (result bool string))
          "Chris can update an article where he is '`Author'."
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
     let* _ = Notes.define_roles ?ctx thomas_note in
     let* _ = Notes.to_authorizable ?ctx chris_note in
     let* _ = Notes.define_roles ?ctx chris_note in
     let* () =
       ActorPermission.create_for_id (snd thomas) Read thomas_note.Notes.id
       |> Backend.ActorPermission.insert ?ctx
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
         (one_of_tuple (Permission.Read, `Note, Some thomas_note.Notes.id))
         (Ok ())
     in
     let%lwt () =
       let set = one_of_tuple (Permission.Read, `Note, None) in
       validate_test
         (fun _ -> pp_error set)
         "Reader cannot read a note of anyone."
         set
         (Error (pp_error set))
     in
     let%lwt () =
       let set =
         one_of_tuple (Permission.Read, `Note, Some chris_note.Notes.id)
       in
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

  let set_author ?ctx (_ : 'a) () =
    (let* aron' = User.to_authorizable ?ctx aron in
     let* chris_article' =
       Article.update_author ?ctx aron' chris_article chris aron
     in
     let* () =
       if chris_article'.Article.author <> aron
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
        ValidationSet.one_of_tuple (Permission.Manage, `User, Some target_uuid)
      in
      Backend.validate ?ctx CCFun.id effects actor
    in
    Alcotest.(check (result unit string))
      "Parametric roles work."
      (Ok ())
      actual
    |> Lwt.return
  ;;

  let transistency_deny ?ctx (_ : 'a) () =
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
          "User cannot edit a post of an article he's manager of."
          (Error
             (Guardian.Utils.deny_message_validation_set
                (snd chris)
                ValidationSet.(
                  one_of_tuple (Update, `Post, Some thomas_chris_post_id)
                  |> show)))
  ;;

  let roles_exist_for_type ?ctx (_ : 'a) () =
    let result =
      let validation_set = ValidationSet.one_of_tuple (Read, `Post, None) in
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

  let test_find_permissions_of_actor ?ctx (_ : 'a) () =
    let sort = CCList.stable_sort PermissionOnTarget.compare in
    let%lwt roles = Backend.ActorRole.permissions_of_actor ?ctx (snd thomas) in
    let expect =
      [ Read, `Article, None
      ; Read, `Post, None
      ; Create, `Article, Some chris_article_id
      ; Update, `Article, Some chris_article_id
      ; Update, `Article, Some thomas_chris_post_id
      ; Update, `Article, Some thomas_note.Notes.id
      ; ( Update
        , `User
        , Some (snd thomas |> Uuid.(Actor.to_string %> Target.of_string_exn)) )
      ; Manage, `Note, Some thomas_note.Notes.id
      ]
      |> CCList.map PermissionOnTarget.of_tuple
    in
    Alcotest.(check (list testable_permission_on_target))
      "return correct list of permissions for actor Aron."
      (expect |> sort)
      (roles |> sort)
    |> Lwt.return
  ;;

  let test_exists_fcn ?ctx:_ (_ : 'a) () =
    let open PermissionOnTarget in
    let target_id = Uuid.Target.create () in
    let read = (Read, `Article, None) |> of_tuple in
    let manage = (Manage, `Article, None) |> of_tuple in
    let update = (Update, `Article, None) |> of_tuple in
    [ true, validate read [ read ], "read in read"
    ; true, validate read [ manage ], "read in manage"
    ; false, validate read [ update ], "read in update"
    ; false, validate manage [ read ], "manage in read"
    ; false, validate update [ read ], "update in read"
    ; ( true
      , validate update [ read; manage; update ]
      , "update in read, manage or update" )
    ; ( true
      , validate
          ((Read, `Article, Some (Uuid.Target.create ())) |> of_tuple)
          [ manage ]
      , "read article id in manage article" )
    ; ( false
      , validate
          ((Read, `Article, Some (Uuid.Target.create ())) |> of_tuple)
          [ (Manage, `Article, Some (Uuid.Target.create ())) |> of_tuple ]
      , "read article id in manage article id (another)" )
    ; ( false
      , validate
          ((Read, `Article, None) |> of_tuple)
          [ (Manage, `Article, Some (Uuid.Target.create ())) |> of_tuple ]
      , "read article in manage article id (another)" )
    ; ( true
      , validate
          ~any_id:true
          ((Read, `Article, Some (Uuid.Target.create ())) |> of_tuple)
          [ (Manage, `Article, Some (Uuid.Target.create ())) |> of_tuple ]
      , "read article id in manage article id (another with 'any_id' active)" )
    ; ( false
      , validate
          ((Read, `Note, Some (Uuid.Target.create ())) |> of_tuple)
          [ manage ]
      , "read note id in manage article" )
    ; ( false
      , validate
          ((Update, `Article, Some target_id) |> of_tuple)
          [ (Read, `Article, Some target_id) |> of_tuple ]
      , "update article is not allowed with read rights" )
    ; ( false
      , validate
          ((Update, `Article, Some target_id) |> of_tuple)
          [ (Update, `Note, Some target_id) |> of_tuple ]
      , "update article is not allowed with update note rights" )
    ; ( true
      , validate
          ((Update, `Article, Some target_id) |> of_tuple)
          [ (Manage, `Article, Some target_id) |> of_tuple ]
      , "update article is allowed with manage rights" )
    ]
    |> CCList.iter (fun (expected, provided, msg) ->
      Alcotest.(
        check
          bool
          (Format.asprintf "Check if permission are correct: %s" msg)
          expected
          provided))
    |> Lwt.return
  ;;

  let test_remove_duplicates ?ctx:_ (_ : 'a) () =
    let open PermissionOnTarget in
    let read = (Read, `Article, None) |> of_tuple in
    let manage = (Manage, `Article, None) |> of_tuple in
    let manage_id =
      (Manage, `Article, Some (Uuid.Target.create ())) |> of_tuple
    in
    let update = (Update, `Article, None) |> of_tuple in
    [ ( [ manage ]
      , remove_duplicates [ read; manage; manage_id ]
      , "remove read and manage_id" )
    ; [ update ], remove_duplicates [ update ], "single enty"
    ]
    |> CCList.iter (fun (expected, provided, msg) ->
      Alcotest.(
        check
          (list testable_permission_on_target)
          (Format.asprintf "Check if permission are correct: %s" msg)
          expected
          provided))
    |> Lwt.return
  ;;

  let test_drop_actor_permission ?ctx (_ : 'a) () =
    let open ActorPermission in
    let open Backend.ActorPermission in
    let actor_permission_id =
      create_for_id (snd thomas) Delete chris_article_id
    in
    let actor_permission_model =
      create_for_model (snd thomas) Delete `Article
    in
    let check ?(available = true) perm =
      let msg =
        Format.asprintf
          "Validate if actor permission is %s"
          (if available then "available" else "absent")
      in
      find_all ?ctx ()
      |> Lwt.map (CCList.exists (equal perm))
      |> Lwt.map (Alcotest.(check bool) msg available)
    in
    (let* () = insert ?ctx actor_permission_id in
     let%lwt () = check actor_permission_id in
     let* () = delete ?ctx actor_permission_id in
     let%lwt () = check ~available:false actor_permission_id in
     let* () = insert ?ctx actor_permission_model in
     let%lwt () = check actor_permission_model in
     let* () = delete ?ctx actor_permission_model in
     let%lwt () = check ~available:false actor_permission_model in
     Lwt.return_ok ())
    >|= Alcotest.(check (result unit string))
          "Read/Delete the actor permissions."
          (Ok ())
  ;;

  let hacker_cannot_update_article ?ctx (_ : 'a) () =
    let%lwt ben =
      Hacker.to_authorizable ?ctx ben |> Lwt.map CCResult.get_or_failwith
    in
    let%lwt try_update =
      Article.update_title ?ctx ben aron_article "Updated Title"
    in
    let () =
      Alcotest.(check bool)
        "Ben cannot update another article."
        (CCResult.is_error try_update)
        true
    in
    Lwt.return_unit
  ;;

  let test_role_assignment_create ?ctx (_ : 'a) () =
    let create_assignable = CCList.map (CCFun.uncurry RoleAssignment.create) in
    let sort = CCList.stable_sort RoleAssignment.compare in
    let admin_objs = [ `Admin, `Reader ] |> create_assignable in
    let author_objs =
      [ `Author, `Editor; `Author, `Reader ] |> create_assignable
    in
    let%lwt () =
      Backend.RoleAssignment.insert ?ctx (admin_objs @ author_objs)
    in
    let%lwt author_assignables =
      Backend.RoleAssignment.find_all_by_role ?ctx `Author
    in
    let () =
      Alcotest.(check (list testable_role_assignment))
        "return correct list of assignable roles for an author"
        (author_objs |> sort)
        (author_assignables |> sort)
    in
    (* Reset all RoleAssignments *)
    let%lwt () =
      let open Lwt in
      let open Backend.RoleAssignment in
      find_all ?ctx ()
      >>= Lwt_list.iter_s (delete ?ctx ~comment:"[system] testing")
    in
    Lwt.return_unit
  ;;

  let test_role_assignment_delete ?ctx (_ : 'a) () =
    let create_assignable = CCList.map (CCFun.uncurry RoleAssignment.create) in
    let sort = CCList.stable_sort RoleAssignment.compare in
    let delete_obj = RoleAssignment.create `Admin `Editor in
    let expected_objs =
      [ `Admin, `Author; `Admin, `Reader ] |> create_assignable
    in
    let%lwt () =
      Backend.RoleAssignment.insert ?ctx (delete_obj :: expected_objs)
    in
    let%lwt () =
      Backend.RoleAssignment.delete ?ctx ~comment:"delete test" delete_obj
    in
    let%lwt admin_objs = Backend.RoleAssignment.find_all_by_role ?ctx `Admin in
    let () =
      Alcotest.(check (list testable_role_assignment))
        "return correct list of assignable roles for an admin"
        (expected_objs |> sort)
        (admin_objs |> sort)
    in
    (* Reset all RoleAssignments *)
    let%lwt () =
      let open Lwt in
      let open Backend.RoleAssignment in
      find_all ?ctx ()
      >>= Lwt_list.iter_s (delete ?ctx ~comment:"[system] testing")
    in
    Lwt.return_unit
  ;;

  let test_role_assignment_can_assign ?ctx (_ : 'a) () =
    let create_assignable = CCList.map (CCFun.uncurry RoleAssignment.create) in
    let admin_objs =
      [ `Admin, `Author; `Admin, `Editor ] |> create_assignable
    in
    let author_objs =
      [ `Author, `Editor; `Author, `Reader ] |> create_assignable
    in
    let%lwt () =
      Backend.RoleAssignment.insert ?ctx (admin_objs @ author_objs)
    in
    let testables =
      [ `Admin, `Reader, false
      ; `Admin, `Author, true
      ; `Author, `Admin, false
      ; `Author, `Reader, true
      ]
    in
    let%lwt () =
      Lwt_list.iter_s
        (fun (role, can_assign, expected) ->
          let%lwt valid =
            Backend.RoleAssignment.can_assign_roles ?ctx role
            |> Lwt.map (CCList.exists (Role.Role.equal can_assign))
          in
          Alcotest.(
            check bool "check if role can assign a target role" expected valid)
          |> Lwt.return)
        testables
    in
    (* Reset all RoleAssignments *)
    let%lwt () =
      let open Lwt in
      let open Backend.RoleAssignment in
      find_all ?ctx ()
      >>= Lwt_list.iter_s (delete ?ctx ~comment:"[system] testing")
    in
    Lwt.return_unit
  ;;
end

let () =
  let make_test_cases ?ctx (module Backend : Guard.PersistenceSig) name =
    let module T = Tests (Backend) in
    let open Alcotest_lwt in
    T.
      [ ( Format.asprintf "(%s) Managing authorizables." name
        , [ test_case
              "Create an authorizable."
              `Quick
              (test_create_authorizable ?ctx)
          ; test_case
              "Retrieve an authorizable."
              `Quick
              (test_find_authorizable ?ctx)
          ] )
      ; ( Format.asprintf "(%s) Managing roles." name
        , [ test_case "Grant a role." `Quick (test_grant_roles ?ctx)
          ; test_case "Revoke a role." `Quick (test_revoke_roles ?ctx)
          ] )
      ; ( Format.asprintf "(%s) Managing authorization rules." name
        , [ test_case "Push rules." `Quick (test_push_role_permission ?ctx)
          ; test_case "Drop rules." `Quick (test_drop_rules ?ctx)
          ; test_case "Read rules." `Quick (test_read_rules ?ctx)
          ; test_case "Save rule." `Quick (save_existing_rule ?ctx)
          ] )
      ; ( Format.asprintf "(%s) Admins should be able to do everything." name
        , [ test_case
              "Update someone else's article."
              `Quick
              (test_admin_update_others' ?ctx)
          ] )
      ; ( Format.asprintf
            "(%s) An authorizable should be able to do everything to entities \
             it owns."
            name
        , [ test_case "Update own article." `Quick (test_update_by_author ?ctx)
          ] )
      ; ( Format.asprintf
            "(%s) An authorizable should be able to do everything to itself."
            name
        , [ test_case "Update" `Quick (can_update_self ?ctx) ] )
      ; ( Format.asprintf
            "(%s) Entities should be denied access to entities they shouldn't \
             access."
            name
        , [ test_case "Cannot update" `Quick (cannot_update ?ctx)
          ; test_case
              "Cannot read"
              `Quick
              (editor_cannot_read_other_article ?ctx)
            (* uncomment the next line to make sure compile-time invariants
               work *)
          ; test_case "Cannot update" `Quick (hacker_cannot_update_article ?ctx)
          ] )
      ; ( Format.asprintf "(%s) Check access for targeted roles." name
        , [ test_case "Editor can edit" `Quick (editor_can_edit ?ctx) ] )
      ; ( Format.asprintf "(%s) Managing ownership." name
        , [ test_case "Set owner" `Quick (set_author ?ctx) ] )
      ; ( Format.asprintf "(%s) Use parametric roles." name
        , [ test_case "Parametric editor role" `Quick (operator_works ?ctx) ] )
      ; ( Format.asprintf
            "(%s) Transistancy, manager of `x` shouldn't be able to update \
             `x.a`."
            name
        , [ test_case "transistency fail" `Quick (transistency_deny ?ctx) ] )
      ; ( Format.asprintf "(%s) Validate returned 'exists' sql for kinds." name
        , [ test_case "transistency" `Quick (roles_exist_for_type ?ctx) ] )
      ; ( Format.asprintf "(%s) Find all actors of a specific role" name
        , [ test_case "role" `Quick (test_find_by_role ?ctx) ] )
      ; ( Format.asprintf "(%s) Find all permissions of actor" name
        , [ test_case "permissions" `Quick (test_find_permissions_of_actor ?ctx)
          ; test_case "validate existance" `Quick (test_exists_fcn ?ctx)
          ; test_case "remove duplicates" `Quick (test_remove_duplicates ?ctx)
          ; test_case
              "Insert/Delete actor permission."
              `Quick
              (test_drop_actor_permission ?ctx)
          ] )
      ; ( Format.asprintf "(%s) Validation for Role assignment" name
        , [ test_case "create" `Quick (test_role_assignment_create ?ctx)
          ; test_case "delete" `Quick (test_role_assignment_delete ?ctx)
          ; test_case "can assign" `Quick (test_role_assignment_can_assign ?ctx)
          ] )
      ]
  in
  let open Guardian_backend.Pools in
  let test_database = "test" in
  let ctx = [ "pool", test_database ] in
  let module MariaConfig = struct
    include DefaultConfig

    let database =
      ( test_database
      , Sys.getenv_opt "DATABASE_URL"
        |> CCOption.get_or ~default:"mariadb://root@database:3306/test" )
    ;;
  end
  in
  let module Database = Make (MariaConfig) in
  let module Maria =
    Guardian_backend.MariaDb.Make (Role.Actor) (Role.Role) (Role.Target)
      (Database)
  in
  Lwt_main.run
  @@
  let () = Database.initialize () in
  let%lwt () = Maria.delete ~ctx () in
  let%lwt () = Maria.migrate ~ctx () in
  let%lwt () = Maria.clean ~ctx () in
  make_test_cases ~ctx (module Maria) "MariadDB Backend"
  |> Alcotest_lwt.run "Authorization"
;;
