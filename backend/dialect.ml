(* The SQL that differs between the databases guardian supports. *)

type kind = Veins.Dialect.kind =
  | Mariadb
  | Postgresql

let uuid_select kind field = Veins.Dialect.uuid_select_fragment kind ~field

let uuid_value kind placeholder =
  Veins.Dialect.uuid_value_fragment kind placeholder
;;

let restore_on_conflict kind columns =
  match kind with
  | Mariadb ->
    {sql|ON DUPLICATE KEY UPDATE mark_as_deleted = NULL, updated_at = NOW()|sql}
  | Postgresql ->
    [%string
      {sql|ON CONFLICT (%{columns}) DO UPDATE SET mark_as_deleted = NULL, updated_at = NOW()|sql}]
;;

let migrations = function
  | Mariadb -> Migrations_mariadb.all
  | Postgresql -> Migrations_postgresql.all
;;

let tables = function
  | Mariadb -> Migrations_mariadb.all_tables
  | Postgresql -> Migrations_postgresql.all_tables
;;
