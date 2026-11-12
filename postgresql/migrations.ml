(* Guardian's schema on PostgreSQL. [permission] is a checked VARCHAR, not an
   enum type: guardian binds it as a string parameter.

   Guardian's schema on PostgreSQL. It starts at the current MariaDB schema
   (see guardian-mariadb's migrations) instead of replaying its history. *)

let set_updated_at_function =
  {sql|
    CREATE OR REPLACE FUNCTION guardian_set_updated_at() RETURNS trigger AS $$
    BEGIN
      NEW.updated_at = NOW();
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql
  |sql}
;;

let create_actors =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_actors (
      id BIGSERIAL PRIMARY KEY,
      uuid UUID NOT NULL UNIQUE,
      model VARCHAR(255) NOT NULL,
      mark_as_deleted TIMESTAMP,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    )
  |sql}
;;

let create_targets =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_targets (
      id BIGSERIAL PRIMARY KEY,
      uuid UUID NOT NULL UNIQUE,
      model VARCHAR(255) NOT NULL,
      mark_as_deleted TIMESTAMP,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      CONSTRAINT unique_uuid_model UNIQUE (uuid, model)
    )
  |sql}
;;

let create_actor_roles =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_actor_roles (
      id BIGSERIAL PRIMARY KEY,
      actor_uuid UUID NOT NULL REFERENCES guardian_actors (uuid),
      role VARCHAR(255) NOT NULL,
      mark_as_deleted TIMESTAMP,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      CONSTRAINT unique_actor_role UNIQUE (actor_uuid, role)
    )
  |sql}
;;

let create_actor_role_targets =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_actor_role_targets (
      id BIGSERIAL PRIMARY KEY,
      actor_uuid UUID NOT NULL REFERENCES guardian_actors (uuid),
      role VARCHAR(255) NOT NULL,
      target_uuid UUID NOT NULL REFERENCES guardian_targets (uuid),
      mark_as_deleted TIMESTAMP,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      CONSTRAINT unique_actor_role_target UNIQUE (actor_uuid, role, target_uuid)
    )
  |sql}
;;

let create_role_permissions =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_role_permissions (
      id BIGSERIAL PRIMARY KEY,
      role VARCHAR(255) NOT NULL,
      permission VARCHAR(16) NOT NULL
        CHECK (permission IN ('create', 'read', 'update', 'delete', 'manage')),
      target_model VARCHAR(255) NOT NULL,
      mark_as_deleted TIMESTAMP,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      CONSTRAINT unique_role_permission_model UNIQUE (role, permission, target_model)
    )
  |sql}
;;

(* NULLS NOT DISTINCT (PostgreSQL 15+): a model-wide permission has no
   target_uuid, and must still conflict with itself, as on MariaDB. *)
let create_actor_permissions =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_actor_permissions (
      id BIGSERIAL PRIMARY KEY,
      actor_uuid UUID NOT NULL REFERENCES guardian_actors (uuid),
      permission VARCHAR(16) NOT NULL
        CHECK (permission IN ('create', 'read', 'update', 'delete', 'manage')),
      target_model VARCHAR(255),
      target_uuid UUID REFERENCES guardian_targets (uuid),
      mark_as_deleted TIMESTAMP,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      CONSTRAINT unique_actor_permission
        UNIQUE NULLS NOT DISTINCT (actor_uuid, permission, target_model, target_uuid)
    )
  |sql}
;;

let create_assign_roles =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_assign_roles (
      id BIGSERIAL PRIMARY KEY,
      role VARCHAR(255) NOT NULL,
      target_role VARCHAR(255) NOT NULL,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      CONSTRAINT unique_role_target_role UNIQUE (role, target_role)
    )
  |sql}
;;

let create_assign_roles_history =
  {sql|
    CREATE TABLE IF NOT EXISTS guardian_assign_roles_history (
      id BIGSERIAL PRIMARY KEY,
      role VARCHAR(255) NOT NULL,
      target_role VARCHAR(255) NOT NULL,
      comment TEXT,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
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

let indexes =
  [ "CREATE INDEX IF NOT EXISTS guardian_actor_roles_role_index ON \
     guardian_actor_roles (role)"
  ; "CREATE INDEX IF NOT EXISTS guardian_actor_role_targets_role_index ON \
     guardian_actor_role_targets (role)"
  ; "CREATE INDEX IF NOT EXISTS guardian_actor_role_targets_target_uuid_index \
     ON guardian_actor_role_targets (target_uuid)"
  ; "CREATE INDEX IF NOT EXISTS guardian_actor_permissions_target_uuid_index \
     ON guardian_actor_permissions (target_uuid)"
  ]
;;

(* MariaDB updates updated_at on every UPDATE (ON UPDATE CURRENT_TIMESTAMP). *)
let updated_at_trigger table =
  [%string
    {sql|
      CREATE OR REPLACE TRIGGER %{table}_updated_at
      BEFORE UPDATE ON %{table}
      FOR EACH ROW EXECUTE FUNCTION guardian_set_updated_at()
    |sql}]
;;

let all =
  [ "create updated_at function", "2026-11-12T00:00", set_updated_at_function
  ; "create guardian actors table", "2026-11-12T00:02", create_actors
  ; "create guardian targets table", "2026-11-12T00:03", create_targets
  ; "create guardian actor roles table", "2026-11-12T00:04", create_actor_roles
  ; ( "create guardian actor role targets table"
    , "2026-11-12T00:05"
    , create_actor_role_targets )
  ; ( "create guardian role permissions table"
    , "2026-11-12T00:06"
    , create_role_permissions )
  ; ( "create guardian actor permissions table"
    , "2026-11-12T00:07"
    , create_actor_permissions )
  ; ( "create guardian assign roles table"
    , "2026-11-12T00:08"
    , create_assign_roles )
  ; ( "create guardian assign roles history table"
    , "2026-11-12T00:09"
    , create_assign_roles_history )
  ]
  @ CCList.mapi
      (fun i sql ->
         Format.asprintf "create index %d" (i + 1), "2026-11-12T00:10", sql)
      indexes
  @ CCList.map
      (fun table ->
         ( "updated_at trigger on " ^ table
         , "2026-11-12T00:11"
         , updated_at_trigger table ))
      all_tables
;;
