(** The SQL that differs between the databases guardian supports, by
    {!Veins.Dialect.kind}. The backend's requests render it for the dialect
    of the connection they run on (see {!Veins.Dialect.request}), so one
    backend serves MariaDB and PostgreSQL (15+). *)

type kind = Veins.Dialect.kind =
  | Mariadb
  | Postgresql

(** [uuid_select kind field] selects the UUID stored in [field] as a lowercase
    dashed string. *)
val uuid_select : kind -> string -> string

(** [uuid_value kind placeholder] turns a dashed UUID parameter into the
    stored representation. *)
val uuid_value : kind -> string -> string

(** [restore_on_conflict kind columns] ends an [INSERT … VALUES (…)] so that a
    row conflicting on the unique [columns] (comma separated) is restored:
    [mark_as_deleted = NULL], [updated_at = NOW()]. *)
val restore_on_conflict : kind -> string -> string

(** [(key, date, sql)] of every schema migration, oldest first. *)
val migrations : kind -> (string * string * string) list

(** All of guardian's tables. *)
val tables : kind -> string list

(** Statement emptying a table. *)
val truncate : kind -> string -> string

(** Statement dropping a table. *)
val drop : kind -> string -> string

(** Statement turning foreign key checks of the session on or off, [None]
    where {!truncate} and {!drop} don't need it. *)
val set_fk_checks : kind -> bool -> string option
