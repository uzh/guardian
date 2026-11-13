module Backend = Backend

(** The backend: [Make (Actor) (Role) (Target) (Database)]. Its SQL follows
    the dialect of each connection, MariaDB / MySQL or PostgreSQL 15+. *)
module Make = Backend.Make

module Needs = Needs
module Database = Database
module Dialect = Dialect
