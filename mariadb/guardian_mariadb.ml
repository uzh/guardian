(** Guardian's MariaDB backend: {!Guardian_backend.Backend.Make} with the
    MariaDB dialect, and the schema migrations. *)

module Migrations = Migrations

module Dialect : Guardian_backend.Dialect.Sig = struct
  let name = "mariadb"

  let uuid_select field =
    Veins.Dialect.uuid_select_fragment Veins.Dialect.Mariadb ~field
  ;;

  let uuid_value = Veins.Dialect.uuid_value_fragment Veins.Dialect.Mariadb

  let restore_on_conflict (_ : string) =
    {sql|ON DUPLICATE KEY UPDATE mark_as_deleted = NULL, updated_at = NOW()|sql}
  ;;

  let migrations = Migrations.all
  let tables = Migrations.all_tables
  let truncate table = [%string "TRUNCATE TABLE %{table}"]
  let drop table = [%string "DROP TABLE IF EXISTS %{table}"]

  let set_fk_checks enabled =
    Some
      (Format.asprintf "SET FOREIGN_KEY_CHECKS = %d" (if enabled then 1 else 0))
  ;;
end

module Make = Guardian_backend.Backend.Make (Dialect)
