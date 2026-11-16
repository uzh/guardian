(* A backend on [Database.Of_veins_explicit]: a guardian function called
   without [?ctx] raises the given exception instead of running on a default
   context. Run these cases last: [clean ~ctx] empties guardian's tables. *)
module Guard = Guardian.Make (Role.Actor) (Role.Role) (Role.Target)

module Make
    (Database : Veins.Pools_sig.Sig)
    (Backend :
       Guard.PersistenceSig with type ('a, 'g) context = ('a, 'g) Veins.Ctx.t) =
struct
  open Guard

  exception Missing_ctx

  module Explicit =
    Guardian_backend.Make (Role.Actor) (Role.Role) (Role.Target)
      (Guardian_backend.Database.Make
         (Guardian_backend.Database.Of_veins_explicit
            (Database)
            (struct
              let missing_ctx = Missing_ctx
            end)))

  type all =
    [ `Read
    | `Write
    | `Schema
    ]

  let owner : all Veins.Role.t = Veins.Role.owner

  let ctx =
    Database.label_ctx
      ~role:owner
      (Veins.Database.label Database.Config.database)
  ;;

  let get_ok = function
    | Ok x -> x
    | Error err -> Alcotest.failf "Unexpected error: %s" err
  ;;

  (* [true] when [f] raises [Missing_ctx], also when it raises before
     returning a promise. *)
  let raises_missing_ctx f =
    Lwt.catch
      (fun () ->
         let%lwt (_ : _) = f () in
         Lwt.return_false)
      (function
        | Missing_ctx -> Lwt.return_true
        | exn -> Lwt.reraise exn)
  ;;

  let test_without_ctx_raises _ () =
    let actor = Actor.create `User (Uuid.Actor.create ()) in
    let%lwt () = Backend.Actor.insert ~ctx actor |> Lwt.map get_ok in
    let%lwt clean_raised = raises_missing_ctx (fun () -> Explicit.clean ()) in
    Alcotest.(check bool) "clean raises Missing_ctx" true clean_raised;
    let%lwt find_raised =
      raises_missing_ctx (fun () -> Explicit.Actor.find actor.Actor.uuid)
    in
    Alcotest.(check bool) "find raises Missing_ctx" true find_raised;
    let%lwt found = Explicit.Actor.find ~ctx actor.Actor.uuid in
    Alcotest.(check bool) "nothing was cleaned" true (CCResult.is_ok found);
    Lwt.return_unit
  ;;

  let test_with_ctx_runs _ () =
    let actor = Actor.create `User (Uuid.Actor.create ()) in
    let%lwt () = Explicit.Actor.insert ~ctx actor |> Lwt.map get_ok in
    let%lwt found = Explicit.Actor.find ~ctx actor.Actor.uuid in
    Alcotest.(check bool) "inserted" true (CCResult.is_ok found);
    let%lwt () = Explicit.clean ~ctx () in
    let%lwt found = Explicit.Actor.find ~ctx actor.Actor.uuid in
    Alcotest.(check bool) "cleaned" true (CCResult.is_error found);
    Lwt.return_unit
  ;;

  let cases =
    let open Alcotest_lwt in
    [ ( "Explicit contexts"
      , [ test_case
            "a call without ?ctx raises the given exception"
            `Quick
            test_without_ctx_raises
        ; test_case "a call with ?ctx runs" `Quick test_with_ctx_runs
        ] )
    ]
  ;;
end
