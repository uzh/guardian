# Changelog

The format is based on [Keep a Changelog](http://keepachangelog.com/) and this project adheres to [Semantic Versioning](http://semver.org/).

## [unreleased](https://github.com/uzh/guardian/tree/HEAD)

### 💥 Breaking: veins, and one package per database

Guardian runs on [veins](https://github.com/uzh/veins) connection pools and contexts, whose design comes from Reynir
Björnsson's database refactoring of the pool tool (uzh/z-pool-tool#647).

### Added

- PostgreSQL 15+ next to MariaDB / MySQL: `Guardian_backend.Make` renders its SQL for the dialect of each connection
  (veins' `Dialect.request`), so one package serves both. `migrate` runs the migrations of the connection's dialect;
  `find_migrations` / `find_clean` take the dialect (`Veins.Dialect.kind`)
- `Guardian_backend.Database.Make`: runs guardian on your own database layer from a small `Core` (`default_ctx`,
  `label`, `is_transaction`, `after_commit`, `query`, `transaction_iter`) over your own `(_, _) ctx` type; guardian
  writes commit or roll back with the caller's transaction. `Database.Of_veins` is that core for a veins pool
- `Guardian_backend.Needs` (`` `Read ``, `` `Write ``, `` `Schema ``, veins' typed needs): every statement declares
  what it needs, and every function taking `?ctx` states it in its type, e.g.
  `validate : ?ctx:(_, [> `Read ]) context -> ...`, so a context whose veins role lacks a right doesn't compile

### Changed

- **Breaking:** the context type is no longer `(string * string) list`. It's `(_, _) context` in
  `Persistence.Backend` (transaction marker and role type), set by the database module, so a caller's own context
  (e.g. `Veins.Ctx.t`) is passed as-is. `ActorSig`/`TargetSig` declare `type (_, _) context`
- **Breaking:** `MakePersistence` returns `PersistenceSig with type ('a, 'g) context = ('a, 'g) Backend.context`; `find_clean`
  returns `(string * string) list`
- **Breaking:** the bundled `Pools` and `MariaDb` modules are gone: use veins and `Guardian_backend.Make`. Requires
  OCaml 5 and caqti 3
- Cache entries are invalidated before a write and again once it's committed, so a concurrent reader can't re-cache
  the old state. Validations inside a transaction bypass the caches
- **Breaking:** `Guardian.Utils.Dynparam` is gone: use `Veins.Dynparam`
- The backend uses veins' `Caqti_encoders` instead of its own copy
- `clean`/`delete` disable foreign key checks on the same connection that runs the statements
- Restoring a deleted role permission works on PostgreSQL (it used MariaDB's `ON DUPLICATE KEY UPDATE`)

### Fixed

- A rolled-back transaction left its pooled connection with autocommit off (caqti-driver-mariadb), so later
  statements on it ran in a transaction that was never committed; veins restores autocommit

## [0.5.0](https://github.com/uzh/guardian/tree/0.4.0) - 2026-07-14

### Added

- Cache for target uuid to model resolution, so cached validations no longer require a database round trip
- Indexes on `role` for `guardian_actor_roles` and `guardian_actor_role_targets`

### Changed

- Cache invalidation for a single actor now bumps a per-actor generation (and a global epoch for full clears) instead of scanning the whole cache; keys are snapshotted when a validation starts so an invalidation that races an in-flight validation is not lost
- Role permission changes invalidate the cache in one step instead of querying and clearing all affected actors
- `PermissionOnTarget.remove_duplicates` runs in linearithmic instead of quadratic time
- `ActorRole.find_by_actor` and `find_by_target` use `UNION ALL`; validation queries use inner joins where applicable
- `clean` and `delete` clear the caches

### Removed

- Redundant single-column indexes that duplicated unique-constraint prefixes

### Fixed

- Version prefix of sexp dependency

## [0.4.0](https://github.com/uzh/guardian/tree/0.4.0) - 2026-04-07

### Added

- GitHub Actions for `lint-fmt` and `lint-opam`
- Backend LRU cache for authentication requests

### Fixed

- Typo in the `additional_pools` argument for the pool `initialize` function

### Changed

- Updated GitHub Actions
- Improved performance of the validation function
- Changed argument from `~matches` to `~required` for the `Permission.is_valid` function for better clarity
- Increased consistency for function outputs: `of_string` always returns an `option`, `_exn` raises exceptions, and `_res` returns a `result`

### Removed

- Deprecated `start` function
- Removed `guardian.opam.locked` file; as this is a library, it should not have a lock file

## [0.3.4](https://github.com/uzh/guardian/tree/0.3.4) - 2025-11-07

### Fixed

- maintenance

## [0.3.3](https://github.com/uzh/guardian/tree/0.3.3) - 2025-04-03

### Fixed

- support for OCaml 5.x

## [0.3.2](https://github.com/uzh/guardian/tree/0.3.2) - 2024-03-21

### Changed

- use string preprocessor instead of defining and using sql functions

### Deprecated

- database `start` function, as no functions need to be defined anymore

## [0.3.1](https://github.com/uzh/guardian/tree/0.3.1) - 2024-01-22

### Added

- role assingment base structure for specifiying which role can assign which ones

### Removed

- old unused database tables

### Fixed

- package deprecation warnings

## [0.3.0](https://github.com/uzh/guardian/tree/0.3.0) - 2023-09-21

### Added

- helper functions on backend for integrated guardian usage

### Changed

- performance update, reduce amount of needed role permissions

### Removed

- relations

## [0.2.0](https://github.com/uzh/guardian/tree/0.2.0) - 2023-07-05

### Added

- helper functions on backend for integrated guardian usage

### Changed

- extract roles into separate table
- rework relations
- combine "find checker" functions
- optimised validate function

## [0.1.1](https://github.com/uzh/guardian/tree/0.1.1) - 2023-03-28

### Added

- test cases for reported bug

### Fixed

- checking for entity and uuid, bug allowed in certain cases to get access when action on entity matched

## [0.1.0](https://github.com/uzh/guardian/tree/0.1.0) - 2023-03-13

💥 The start of using the changelog. Preparations for our version 0.1.0.

### Added

- deployment for new releases and documentation

### Changed

- typified the guardian library
- restructure the api
