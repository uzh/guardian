(** The capabilities guardian's statements declare (veins, see
    {!Veins.Capability}).

    On a database with one URL per role, give the role guardian runs as the
    ones it needs, e.g.
    [Veins.Role.create "app" ~capabilities:(app_caps @ Guardian_backend.Capabilities.[ read; write ])];
    migrations, [clean] and [delete] need {!schema}. A single-URL database has
    all of them. *)

(** Reading actors, targets, roles and permissions. *)
val read : Veins.Capability.t

(** Inserting, updating and deleting them. *)
val write : Veins.Capability.t

(** Creating, truncating and dropping guardian's tables. *)
val schema : Veins.Capability.t

(** [[ read; write; schema ]]. *)
val all : Veins.Capability.t list
