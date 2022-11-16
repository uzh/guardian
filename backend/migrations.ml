let create_guardian_actors_table_sql =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_actors (
      id TEXT UNIQUE NOT NULL,
      roles TEXT NOT NULL,
      parent TEXT
    )
  |sql}
;;

let create_guardian_targets_table_sql =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_targets (
      id TEXT UNIQUE NOT NULL,
      roles TEXT NOT NULL,
      parent TEXT
    )
  |sql}
;;

let create_guardian_rules_table_sql =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_rules (
      actor_id TEXT,
      actor_role TEXT,
      act TEXT NOT NULL,
      target_id TEXT,
      target_role TEXT,
      -- These constraints are necessary to prevent rules that cannot be
      -- represented within OCaml.
      CONSTRAINT only_one_actor
          CHECK(
              (actor_id IS NULL OR actor_role IS NULL)
              and (actor_id IS NOT NULL OR actor_role IS NOT NULL)
          ),
      CONSTRAINT only_one_target
          CHECK(
              (target_id IS NULL OR target_role IS NULL)
              and (target_id IS NOT NULL OR target_role IS NOT NULL)
          ),
      UNIQUE(actor_role, act, target_role),
      UNIQUE(actor_role, act, target_id),
      UNIQUE(actor_id, act, target_role),
      UNIQUE(actor_id, act, target_id)
    )
  |sql}
;;

let all_tables = [ "guardian_actors"; "guardian_targets"; "guardian_rules" ]

let all =
  [ ( "create guardian actors table"
    , "2022-10-28T11:30"
    , create_guardian_actors_table_sql )
  ; ( "create guardian rule table"
    , "2022-10-28T11:31"
    , create_guardian_rules_table_sql )
  ; ( "create guardian targets table"
    , "2022-11-14T11:30"
    , create_guardian_targets_table_sql )
  ]
;;
