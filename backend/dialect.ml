(** The SQL differences between the databases guardian supports. The backend
    ({!Backend.Make}) is written against this; [guardian-mariadb] and
    [guardian-postgresql] provide the implementations. *)

module type Sig = sig
  (** Short name, used for log sources, e.g. ["mariadb"]. *)
  val name : string

  (** [uuid_select field] selects the UUID stored in [field] as a lowercase
      dashed string. *)
  val uuid_select : string -> string

  (** [uuid_value placeholder] turns a dashed UUID parameter into the stored
      representation. *)
  val uuid_value : string -> string

  (** [restore_on_conflict columns] ends an [INSERT … VALUES (…)] so that a row
      conflicting on the unique [columns] (comma separated) is restored:
      [mark_as_deleted = NULL], [updated_at = NOW()]. *)
  val restore_on_conflict : string -> string

  (** [(key, date, sql)] of every schema migration, oldest first. *)
  val migrations : (string * string * string) list

  (** All of guardian's tables. *)
  val tables : string list

  (** Statement emptying a table. *)
  val truncate : string -> string

  (** Statement dropping a table. *)
  val drop : string -> string

  (** Statement turning foreign key checks of the session on or off, [None]
      where {!truncate} and {!drop} don't need it. *)
  val set_fk_checks : bool -> string option
end
