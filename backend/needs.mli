(** The rights guardian's statements need (veins, see {!Veins.Need}).

    Guardian's functions state them in their types: e.g. [validate] takes a
    context whose role grants [`Read], [ActorRole.upsert] one granting
    [`Read | `Write], and [migrate] one granting [`Schema]. The tags are
    plain polymorphic variants, so an application's roles grant them by
    listing them, e.g.
    [let query : [ `Read ] Veins.Role.t = Veins.Role.v ~login "query"]. *)

(** Reading actors, targets, roles and permissions. *)
val read : [> `Read ] Veins.Need.t

(** Inserting, updating and deleting them. *)
val write : [> `Write ] Veins.Need.t

(** Creating, truncating and dropping guardian's tables. *)
val schema : [> `Schema ] Veins.Need.t
