let create_guardian_actors_table_sql =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_actors (
      id bigint(20) unsigned NOT NULL AUTO_INCREMENT,
      uuid binary(16) UNIQUE NOT NULL,
      roles TEXT NOT NULL,
      owner binary(16) NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id)
    )
  |sql}
;;

let create_guardian_targets_table_sql =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_targets (
      id bigint(20) unsigned NOT NULL AUTO_INCREMENT,
      uuid binary(16) UNIQUE NOT NULL,
      kind varchar(255) NOT NULL,
      owner binary(16),
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      -- Following constraint already handled with a unique id
      -- CONSTRAINT unique_id_kind UNIQUE (uuid, kind),
      PRIMARY KEY (id)
    )
  |sql}
;;

let create_guardian_rules_table_sql =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_rules (
      id bigint(20) unsigned NOT NULL AUTO_INCREMENT,
      actor_role varchar(255) NOT NULL,
      actor_uuid binary(16) NULL,
      act ENUM('create', 'read', 'update', 'delete', 'manage') NOT NULL,
      target_role varchar(255) NOT NULL,
      target_uuid binary(16) NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      CONSTRAINT actor_act_target UNIQUE (actor_role, actor_uuid, act, target_role, target_uuid),
      PRIMARY KEY (id)
    )
  |sql}
;;

let create_guardian_relations_table =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_relations (
      id bigint(20) unsigned NOT NULL AUTO_INCREMENT,
      origin varchar(255) NOT NULL,
      target varchar(255) NOT NULL,
      query text NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      CONSTRAINT unique_origin_target UNIQUE (origin, target),
      PRIMARY KEY (id)
    )
  |sql}
;;

let create_guardian_actor_roles_table_sql =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_actor_roles (
      id bigint(20) unsigned NOT NULL AUTO_INCREMENT,
      actor_uuid binary(16) NOT NULL,
      role varchar(255) NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      CONSTRAINT unique_actor_role UNIQUE (actor_uuid, role),
      PRIMARY KEY (id)
    )
  |sql}
;;

let remove_actor_roles_column =
  {sql|
    ALTER TABLE guardian_actors
    DROP COLUMN IF EXISTS roles
  |sql}
;;

let all_tables =
  [ "guardian_actors"
  ; "guardian_actor_roles"
  ; "guardian_targets"
  ; "guardian_rules"
  ; "guardian_relations"
  ]
;;

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
  ; ( "create guardian relations table"
    , "2023-05-03T08:30"
    , create_guardian_relations_table )
  ; ( "create guardian actor roles table"
    , "2023-05-09T10:30"
    , create_guardian_actor_roles_table_sql )
  ; ( "remove roles from guardian actors table"
    , "2023-05-09T10:31"
    , remove_actor_roles_column )
  ]
;;
