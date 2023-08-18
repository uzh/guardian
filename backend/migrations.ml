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

let add_and_change_column_of_actors =
  {sql|
    ALTER TABLE guardian_actors
    ADD COLUMN model varchar(255) AFTER uuid,
    ADD COLUMN mark_as_deleted DATETIME AFTER owner_uuid,
    CHANGE owner owner_uuid binary(16)
  |sql}
;;

let add_column_to_actor_roles =
  {sql|
    ALTER TABLE guardian_actor_roles
    ADD mark_as_deleted DATETIME AFTER target_uuid
    ADD CONSTRAINT unique_actor_role UNIQUE (actor_uuid, role)
  |sql}
;;

let add_and_change_column_of_targets =
  {sql|
    ALTER TABLE guardian_targets
    ADD mark_as_deleted DATETIME,
    CHANGE kind model varchar(255),
    CHANGE owner owner_uuid binary(16)
  |sql}
;;

let create_guardian_actor_permissions_table =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_actor_permissions (
      id bigint(20) unsigned NOT NULL AUTO_INCREMENT,
      actor_uuid binary(16) NOT NULL,
      permission ENUM('create', 'read', 'update', 'delete', 'manage') NOT NULL,
      target_model varchar(255) NULL,
      target_uuid binary(16) NULL,
      mark_as_deleted DATETIME,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      CONSTRAINT unique_actor_permission UNIQUE (actor_uuid, permission, target_model, target_uuid),
      PRIMARY KEY (id)
    )
  |sql}
;;

let drop_relations = {sql|DROP TABLE IF EXISTS guardian_relations|sql}

let rename_rule_table =
  {sql|
    ALTER TABLE guardian_rules
    RENAME TO guardian_role_permissions
  |sql}
;;

let change_columns_of_role_permissions =
  {sql|
    ALTER TABLE guardian_role_permissions
    ADD mark_as_deleted DATETIME AFTER target_uuid,
    CHANGE actor_role role varchar(255),
    CHANGE target_role target_model varchar(255),
    CHANGE act permission ENUM('create', 'read', 'update', 'delete', 'manage'),
    DROP CONSTRAINT actor_act_target,
    DROP COLUMN actor_uuid,
    ADD CONSTRAINT role_permission_model_target UNIQUE (role, permission, target_model, target_uuid)
  |sql}
;;

let create_guardian_actor_role_targets_table =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_actor_role_targets (
      id bigint(20) unsigned NOT NULL AUTO_INCREMENT,
      actor_uuid binary(16) NOT NULL,
      role varchar(255) NOT NULL,
      target_uuid binary(16) NOT NULL,
      mark_as_deleted DATETIME,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      CONSTRAINT unique_actor_role_target UNIQUE (actor_uuid, role, target_uuid),
      PRIMARY KEY (id)
    )
  |sql}
;;

let all_tables =
  [ "guardian_actors"
  ; "guardian_actor_roles"
  ; "guardian_targets"
  ; "guardian_role_permissions"
  ; "guardian_actor_permissions"
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
  ; ( "add model and mark_as_deleted to guardian actors table"
    , "2023-08-10T16:45"
    , add_and_change_column_of_actors )
  ; ( "add target_uuid and mark_as_deleted to guardian actor roles table"
    , "2023-08-10T16:46"
    , add_column_to_actor_roles )
  ; ( "add mark_as_deleted and rename kind to model in guardian targets table"
    , "2023-08-10T16:47"
    , add_and_change_column_of_targets )
  ; ( "create guardian actor permissions table"
    , "2023-08-10T16:48"
    , create_guardian_actor_permissions_table )
  ; ( "rename guardian rules table to guardian role permissions table"
    , "2023-08-10T16:49"
    , rename_rule_table )
  ; ( "change columns of guardian role permissions table"
    , "2023-08-10T16:50"
    , change_columns_of_role_permissions )
  ; "drop guardian relations table", "2023-08-10T16:51", drop_relations
  ]
;;
