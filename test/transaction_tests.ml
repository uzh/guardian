(* Guardian statements running on the caller's transaction, and the caches
   staying correct across commit and rollback. *)
module Guard = Guardian.Make (Role.Actor) (Role.Role) (Role.Target)

module Make
    (Database : Veins.Pools_sig.Sig)
    (Backend :
       Guard.PersistenceSig with type ('a, 'g) context = ('a, 'g) Veins.Ctx.t) =
struct
  open Guard

  exception Rollback

  (* Every right guardian's statements need: the tests run as the owner. *)
  type all =
    [ `Read
    | `Write
    | `Schema
    ]

  let owner : all Veins.Role.t = Veins.Role.owner
  let label = Veins.Database.label (Database.Config.database ())
  let ctx = Database.label_ctx ~role:owner label

  let get_ok = function
    | Ok x -> x
    | Error err -> Alcotest.failf "Unexpected error: %s" err
  ;;

  (* Runs [fcn] in a transaction that is always rolled back. *)
  let rolled_back fcn =
    try%lwt
      Database.transaction_ctx ~role:owner label (fun ctx ->
        let%lwt () = fcn ctx in
        Lwt.fail Rollback)
    with
    | Rollback -> Lwt.return_unit
  ;;

  (* A fresh user and article; [`Admin] may update every article. *)
  let setup () =
    let actor = Actor.create `User (Uuid.Actor.create ()) in
    let target = Target.create `Article (Uuid.Target.create ()) in
    let%lwt () = Backend.Actor.insert ~ctx actor |> Lwt.map get_ok in
    let%lwt () = Backend.Target.insert ~ctx target |> Lwt.map get_ok in
    let%lwt (_ : (RolePermission.t list, RolePermission.t list) result) =
      Backend.RolePermission.insert_all
        ~ctx
        [ RolePermission.create `Admin Permission.Manage `Article ]
    in
    Lwt.return (actor, target)
  ;;

  let can_update ~ctx actor target =
    Backend.validate
      ~ctx
      CCFun.id
      (ValidationSet.one_of_tuple
         (Permission.Update, `Article, Some target.Target.uuid))
      actor
    |> Lwt.map CCResult.is_ok
  ;;

  let admin_of actor = ActorRole.create actor.Actor.uuid `Admin

  let test_rollback_discards_writes _ () =
    let%lwt actor, target = setup () in
    (* warm the cache with "denied" *)
    let%lwt before = can_update ~ctx actor target in
    Alcotest.(check bool) "denied before" false before;
    let%lwt () =
      rolled_back (fun tctx ->
        let%lwt () = Backend.ActorRole.upsert ~ctx:tctx (admin_of actor) in
        let%lwt inside = can_update ~ctx:tctx actor target in
        Alcotest.(check bool) "own uncommitted grant is visible" true inside;
        Lwt.return_unit)
    in
    let%lwt roles = Backend.ActorRole.find_by_actor ~ctx actor.Actor.uuid in
    Alcotest.(check int) "no role row after rollback" 0 (CCList.length roles);
    let%lwt after = can_update ~ctx actor target in
    Alcotest.(check bool) "still denied after rollback" false after;
    Lwt.return_unit
  ;;

  (* A reader on another connection caches the old, committed state while the
     transaction is open. The commit must invalidate that entry. *)
  let test_commit_invalidates_concurrent_reads _ () =
    let%lwt actor, target = setup () in
    let%lwt () =
      Database.transaction_ctx ~role:owner label (fun tctx ->
        let%lwt () = Backend.ActorRole.upsert ~ctx:tctx (admin_of actor) in
        let%lwt outside = can_update ~ctx actor target in
        Alcotest.(check bool) "not visible to other connections" false outside;
        Lwt.return_unit)
    in
    let%lwt after = can_update ~ctx actor target in
    Alcotest.(check bool) "granted right after commit" true after;
    Lwt.return_unit
  ;;

  (* [RoleAssignment.delete] runs its own [transaction_iter]: inside a
     transaction it must join it, not commit on its own. *)
  let test_nested_transaction_rolls_back _ () =
    let assignment = RoleAssignment.create `Editor `Reader in
    let%lwt () = Backend.RoleAssignment.insert ~ctx [ assignment ] in
    let%lwt () =
      rolled_back (fun tctx ->
        Backend.RoleAssignment.delete ~ctx:tctx assignment)
    in
    let%lwt assignments =
      Backend.RoleAssignment.find_all_by_role ~ctx `Editor
    in
    Alcotest.(check bool)
      "assignment survives the rollback"
      true
      (CCList.mem ~eq:RoleAssignment.equal assignment assignments);
    Backend.RoleAssignment.delete ~ctx assignment
  ;;

  let test_after_commit_dropped_on_rollback _ () =
    let ran = ref false in
    let%lwt () =
      rolled_back (fun tctx ->
        Veins.Ctx.after_commit tctx (fun () -> ran := true);
        Lwt.return_unit)
    in
    Alcotest.(check bool) "hook not run" false !ran;
    let%lwt () =
      Database.transaction_ctx ~role:owner label (fun tctx ->
        Veins.Ctx.after_commit tctx (fun () -> ran := true);
        Alcotest.(check bool) "hook deferred until commit" false !ran;
        Lwt.return_unit)
    in
    Alcotest.(check bool) "hook run after commit" true !ran;
    Lwt.return_unit
  ;;

  let cases =
    let open Alcotest_lwt in
    [ ( "Transactions"
      , [ test_case
            "rollback discards guardian writes and cache entries"
            `Quick
            test_rollback_discards_writes
        ; test_case
            "commit invalidates entries cached by concurrent readers"
            `Quick
            test_commit_invalidates_concurrent_reads
        ; test_case
            "nested transaction_iter joins the outer transaction"
            `Quick
            test_nested_transaction_rolls_back
        ; test_case
            "after_commit hooks run on commit only"
            `Quick
            test_after_commit_dropped_on_rollback
        ] )
    ]
  ;;
end
