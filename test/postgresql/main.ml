(* Guardian's test cases against PostgreSQL Backend. Set POSTGRES_URL to a throwaway
   database: the tests drop and recreate guardian's tables. *)

let () =
  let test_database = Veins.Label.create "test" in
  let module Database = Veins_postgresql.Pools.Make (struct
      let database =
        Veins.Database.create
          test_database
          (Sys.getenv_opt "POSTGRES_URL"
           |> CCOption.get_or
                ~default:"postgresql://root:root@database-postgres/test"
           |> Veins.Url.create)
      ;;
    end)
  in
  let module Backend =
    Guardian_backend.Make (Role.Actor) (Role.Role) (Role.Target)
      (Guardian_backend.Database.Make
         (Guardian_backend.Database.Of_veins
            (Database)
            (struct
              let default_role = Veins.Role.owner
            end)))
  in
  let ctx = Database.label_ctx ~role:Veins.Role.owner test_database in
  Lwt_main.run
  @@
  let () = Database.Pool.initialize () in
  let%lwt () = Backend.delete ~ctx () in
  let%lwt () = Backend.migrate ~ctx () in
  let%lwt () = Backend.clean ~ctx () in
  let module Transactions = Transaction_tests.Make (Database) (Backend) in
  let module Cases = Suite.Test_cases (Backend) in
  Suite.role_model_tests
  @ Cases.make ~ctx "PostgreSQL Backend"
  @ Transactions.cases
  |> Alcotest_lwt.run "Authorization"
;;
