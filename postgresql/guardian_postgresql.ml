(** Guardian's PostgreSQL backend (PostgreSQL 15+):
    {!Guardian_backend.Backend.Make} with the PostgreSQL dialect, and the
    schema migrations. *)

module Migrations = Migrations

module Dialect : Guardian_backend.Dialect.Sig = struct
  let name = "postgresql"

  let uuid_select field =
    Veins.Dialect.uuid_select_fragment Veins.Dialect.Postgresql ~field
  ;;

  let uuid_value = Veins.Dialect.uuid_value_fragment Veins.Dialect.Postgresql

  let restore_on_conflict columns =
    [%string
      {sql|ON CONFLICT (%{columns}) DO UPDATE SET mark_as_deleted = NULL, updated_at = NOW()|sql}]
  ;;

  let migrations = Migrations.all
  let tables = Migrations.all_tables
  let truncate table = [%string "TRUNCATE TABLE %{table} CASCADE"]
  let drop table = [%string "DROP TABLE IF EXISTS %{table} CASCADE"]
  let set_fk_checks (_ : bool) = None
end

module Make = Guardian_backend.Backend.Make (Dialect)
