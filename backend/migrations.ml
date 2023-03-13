let create_guardian_actors_table_sql =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_actors (
      id binary(16) UNIQUE NOT NULL,
      roles TEXT NOT NULL,
      owner binary(16) NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
    )
  |sql}
;;

let create_guardian_targets_table_sql =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_targets (
      id binary(16) UNIQUE NOT NULL,
      kind varchar(255) NOT NULL,
      owner binary(16),
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
      -- Following constraint already handled with a unique id
      -- CONSTRAINT unique_id_kind UNIQUE (id, kind)
    )
  |sql}
;;

let create_guardian_rules_table_sql =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_rules (
      actor_role varchar(255) NOT NULL,
      actor_id binary(16) NULL,
      act ENUM('create', 'read', 'update', 'delete', 'manage') NOT NULL,
      target_role varchar(255) NOT NULL,
      target_id binary(16) NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      CONSTRAINT actor_act_target UNIQUE (actor_role, actor_id, act, target_role, target_id)
    )
  |sql}
;;

let all_tables = [ "guardian_actors"; "guardian_targets"; "guardian_rules" ]

let all =
  [ ( "create guardian actors table"
    , "2023-03-09T17:00"
    , create_guardian_actors_table_sql )
  ; ( "create guardian rule table"
    , "2023-03-09T17:01"
    , create_guardian_rules_table_sql )
  ; ( "create guardian targets table"
    , "2023-03-09T17:02"
    , create_guardian_targets_table_sql )
  ]
;;
