let create_guardian_actors_table =
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

let create_guardian_targets_table =
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

let create_guardian_rules_table =
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

let create_guardian_actor_roles_table =
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

let change_table_names =
  {sql|
    RENAME TABLE
      guardian_actors TO guardian_actors_old,
      guardian_actor_roles TO guardian_actor_roles_old,
      guardian_relations TO guardian_relations_old,
      guardian_rules TO guardian_rules_old,
      guardian_targets TO guardian_targets_old
  |sql}
;;

let create_v2_guardian_actors_table =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_actors (
      id bigint(20) unsigned NOT NULL AUTO_INCREMENT,
      uuid binary(16) UNIQUE NOT NULL,
      model varchar(255) NOT NULL,
      mark_as_deleted DATETIME,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      INDEX guardian_actors_uuid_index (uuid)
    )
  |sql}
;;

let create_v2_guardian_targets_table =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_targets (
      id bigint(20) unsigned NOT NULL AUTO_INCREMENT,
      uuid binary(16) UNIQUE NOT NULL,
      model varchar(255) NOT NULL,
      mark_as_deleted DATETIME,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      CONSTRAINT unique_uuid_model UNIQUE (uuid, model),
      PRIMARY KEY (id),
      INDEX guardian_targets_uuid_index (uuid)
    )
  |sql}
;;

let create_v2_guardian_actor_roles_table =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_actor_roles (
      id bigint(20) unsigned NOT NULL AUTO_INCREMENT,
      actor_uuid binary(16) NOT NULL,
      role varchar(255) NOT NULL,
      mark_as_deleted DATETIME,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      CONSTRAINT unique_actor_role UNIQUE (actor_uuid, role),
      CONSTRAINT fk_actor_roles_actor_uuid FOREIGN KEY (actor_uuid) REFERENCES guardian_actors (uuid),
      PRIMARY KEY (id),
      INDEX guardian_actor_roles_actor_uuid_index (actor_uuid)
    )
  |sql}
;;

let create_v2_guardian_actor_role_targets_table =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_actor_role_targets (
      id bigint(20) unsigned NOT NULL AUTO_INCREMENT,
      actor_uuid binary(16) NOT NULL,
      role varchar(255) NOT NULL,
      target_uuid binary(16) NOT NULL,
      mark_as_deleted DATETIME,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      CONSTRAINT unique_actor_role UNIQUE (actor_uuid, role, target_uuid),
      CONSTRAINT fk_actor_role_targets_actor_uuid FOREIGN KEY (actor_uuid) REFERENCES guardian_actors (uuid),
      CONSTRAINT fk_actor_role_targets_target_uuid FOREIGN KEY (target_uuid) REFERENCES guardian_targets (uuid),
      PRIMARY KEY (id),
      INDEX guardian_actor_role_targets_actor_uuid_index (actor_uuid),
      INDEX guardian_actor_role_targets_target_uuid_index (target_uuid)
    )
  |sql}
;;

let create_v2_guardian_role_permissions_table =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_role_permissions (
      id bigint(20) unsigned NOT NULL AUTO_INCREMENT,
      role varchar(255) NOT NULL,
      permission ENUM('create', 'read', 'update', 'delete', 'manage') NOT NULL,
      target_model varchar(255) NOT NULL,
      mark_as_deleted DATETIME,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      CONSTRAINT unique_role_permission_model UNIQUE (role, permission, target_model),
      PRIMARY KEY (id)
    )
  |sql}
;;

let create_v2_guardian_actor_permissions_table =
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
      CONSTRAINT fk_actor_permissions_actor_uuid FOREIGN KEY (actor_uuid) REFERENCES guardian_actors (uuid),
      CONSTRAINT fk_actor_permissions_target_uuid FOREIGN KEY (target_uuid) REFERENCES guardian_targets (uuid),
      PRIMARY KEY (id),
      INDEX guardian_actor_permissions_actor_uuid_index (actor_uuid),
      INDEX guardian_actor_permissions_target_uuid_index (target_uuid)
    )
  |sql}
;;

let drop_relations = {sql|DROP TABLE IF EXISTS guardian_relations|sql}
let drop_old_actors = {sql|DROP TABLE IF EXISTS guardian_actors_old|sql}

let drop_old_actor_roles =
  {sql|DROP TABLE IF EXISTS guardian_actor_roles_old|sql}
;;

let drop_old_targets = {sql|DROP TABLE IF EXISTS guardian_targets_old|sql}
let drop_old_relations = {sql|DROP TABLE IF EXISTS guardian_relations_old|sql}
let drop_old_rules = {sql|DROP TABLE IF EXISTS guardian_rules_old|sql}

let create_guardian_assign_roles_table =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_assign_roles (
      id bigint(20) unsigned NOT NULL AUTO_INCREMENT,
      role varchar(255) NOT NULL,
      target_role varchar(255) NOT NULL,
      mark_as_deleted DATETIME,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      CONSTRAINT unique_role_target_role UNIQUE (role, target_role),
      PRIMARY KEY (id)
    )
  |sql}
;;

let create_guardian_assign_roles_history_table =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_assign_roles_history (
      id bigint(20) unsigned NOT NULL AUTO_INCREMENT,
      role varchar(255) NOT NULL,
      target_role varchar(255) NOT NULL,
      comment text NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id)
    )
  |sql}
;;

let all_tables =
  [ "guardian_actors"
  ; "guardian_actor_roles"
  ; "guardian_actor_role_targets"
  ; "guardian_targets"
  ; "guardian_role_permissions"
  ; "guardian_actor_permissions"
  ; "guardian_assign_roles"
  ; "guardian_assign_roles_history"
  ]
;;

let all =
  [ ( "create guardian actors table"
    , "2023-03-09T17:00"
    , create_guardian_actors_table )
  ; ( "create guardian rule table"
    , "2023-03-09T17:01"
    , create_guardian_rules_table )
  ; ( "create guardian targets table"
    , "2023-03-09T17:02"
    , create_guardian_targets_table )
  ; ( "create guardian relations table"
    , "2023-05-03T08:30"
    , create_guardian_relations_table )
  ; ( "create guardian actor roles table"
    , "2023-05-09T10:30"
    , create_guardian_actor_roles_table )
  ; ( "remove roles from guardian actors table"
    , "2023-05-09T10:31"
    , remove_actor_roles_column )
  ; "change table names", "2023-08-18T15:15", change_table_names
  ; ( "create v2 guardian actors table"
    , "2023-08-18T15:16"
    , create_v2_guardian_actors_table )
  ; ( "create v2 guardian targets table"
    , "2023-08-18T15:17"
    , create_v2_guardian_targets_table )
  ; ( "create v2 guardian actor roles table"
    , "2023-08-18T15:18"
    , create_v2_guardian_actor_roles_table )
  ; ( "create v2 guardian actor role targets table"
    , "2023-08-18T15:19"
    , create_v2_guardian_actor_role_targets_table )
  ; ( "create v2 guardian role permissions table"
    , "2023-08-18T15:20"
    , create_v2_guardian_role_permissions_table )
  ; ( "create v2 guardian actor permissions table"
    , "2023-08-18T15:21"
    , create_v2_guardian_actor_permissions_table )
  ; "drop guardian relations table", "2023-08-18T15:22", drop_relations
  ; "drop old guardian actors table", "2024-01-17T09:00", drop_old_actors
  ; ( "drop old guardian actor roles table"
    , "2024-01-17T09:01"
    , drop_old_actor_roles )
  ; "drop old guardian targets table", "2024-01-17T09:02", drop_old_targets
  ; "drop old guardian relations table", "2024-01-17T09:03", drop_old_relations
  ; "drop old guardian rules table", "2024-01-17T09:04", drop_old_rules
  ; ( "create guardian assign roles table"
    , "2024-01-18T16:00"
    , create_guardian_assign_roles_table )
  ; ( "create guardian assign roles history table"
    , "2024-01-18T16:01"
    , create_guardian_assign_roles_history_table )
  ]
;;
