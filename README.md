# Guardian

Generic framework for roles and permissions to be used in our projects

## Packages

- `guardian`: roles, permissions and validation (`Guardian`), plus the database-independent backend
  (`Guardian_backend`) running on [veins](https://github.com/uzh/veins) connection pools
- `guardian-mariadb`: the MariaDB / MySQL backend (`Guardian_mariadb.Make`)
- `guardian-postgresql`: the PostgreSQL backend (`Guardian_postgresql.Make`, PostgreSQL 15+)

## Setup

Guardian runs its statements on your veins pool and on the context you pass as `?ctx`: a label, a held connection
or an open transaction, so guardian writes commit or roll back with yours.

```ocaml
module Db = Veins_mariadb.Pools.Make (struct
  let database =
    Veins.Database.create (Veins.Label.create "app")
      (Veins.Database.Main (Veins.Url.create "mariadb://root@database:3306/dev"))
end)

module Guard =
  Guardian_mariadb.Make (Roles.Actor) (Roles.Role) (Roles.Target)
    (Guardian_backend.Database.Make
       (Guardian_backend.Database.Of_veins (Db) (struct let default_role = None end)))

let () = Db.Pool.initialize ()
let%lwt () = Guard.migrate ~ctx:(Db.label_ctx (Veins.Label.create "app")) ()
(* To integrate the migrations into your application's migration state, see
   [Guard.find_migrations]. *)
```

Every statement declares the capabilities it needs (`Guardian_backend.Capabilities`: `read`, `write`, `schema`). On a
veins database with one URL per role, the role guardian runs as must have them; a single-URL database has all of
them. To run guardian on a database layer other than veins, implement `Guardian_backend.Database.Core`.

## Usage

The `test` directory shows an example implementation of how guardian can be used.

- `role.ml`: Definition of actors and targets
- `role.mli`: Signature of the defined actors and targets
- `guard.ml`: Create the guardian service
- `article.ml`: Definition of the article target
- `hacker.ml`: Definition of the hacker actor
- `user.ml`: Definition of the user actor and target
- `main.ml`: implementation of all test cases

Example usage:

```ocaml
  module Guard = Guardian.Make (Role.Actor) (Role.Target)

  let thomas = "Thomas", Guard.Uuid.Actor.create ()
  let mike = "Mike", Guard.Uuid.Actor.create ()

  let thomas_article = Article.make "Foo" "Bar" thomas
  let mike_article = Article.make "Hello" "World" mike

  let example_rule = `Actor (snd mike), `Update, `Target thomas_article.uuid

  let initialize_authorizables_and_rules ?ctx =
    (* Note: As a user can be an actor and a target, both need to be initialized *)
    let* (_: [> `User ] MariaDb.actor) = User.to_authorizable ?ctx thomas in
    let* (_: [> `User ] MariaDb.actor) = User.to_authorizable ?ctx mike in
    let* (_: [> `User ] MariaDb.target) = UserTarget.to_authorizable ?ctx thomas in
    let* (_: [> `User ] MariaDb.target) = UserTarget.to_authorizable ?ctx mike in
    let* (_: [> `Article ] MariaDb.target) = Article.to_authorizable ?ctx thomas_article in
    let* (_: [> `Article ] MariaDb.target) = Article.to_authorizable ?ctx mike_article in
    let* () = MariaDb.Rule.save ?ctx example_role in
    Lwt.return_unit

  (* let mike Update the title of thomas article -> returns a (Article.t, string) Lwt_result.t  *)
  let update_title = Article.update_title ?ctx mike thomas_article "Updated Title"
```

## Development

This project runs in a DevContainer. All credentials are passed via a local `.env` file — no host SSH keys required.

1. Follow the general DevContainer instructions in the [Engineering Wiki](https://gitlab.uzh.ch/econ/it/engineering/-/wikis/2_best-practices/DevContainer).
1. Copy the env template and fill in the values: `cp .devcontainer/.env.example .devcontainer/.env`
1. Click the `><` icon in the bottom-left corner of VS Code and select **Reopen in Container**.

> **NOTE**: When the setup is fully installed, select `View` -> `Command Palette...` and run the command `OCaml: Restart Language Server`

### Database View

There is an `Adminer` container added to the development package. To be able to use it, follow these few steps:

1. Uncomment its line in the `.devcontainer/devcontainer.json` under `runServices`
1. Use `Remote-Containers: Rebuild Container` that it will also create and startup the `Adminer` container
1. Open your web browser and open `localhost:8080`
1. Use the _development_ credentials defined for the development database in your `.env` file
    (MariaDB: `server: database-mariadb`, `user: admin`, `password: password`; PostgreSQL: `server: database-postgres`,
    `user: root`, `password: root`; databases `development` and `test`)

### Commands

Most used commands can be found in the following list. For the full list of commands, checkout the `Makefile`.

- `make build` - to build the project
- `make build-watch` - to build and watch the project
- `make test` - to run all tests. This requires a running MariaDB (`MARIADB_URL`) and PostgreSQL (`POSTGRES_URL`) instance; the tests drop guardian's tables, use throwaway databases.

## 🚀 Release new version

1. Update `CHANGELOG.md` and document changes made. Ensure the version to be releases has a header matching the version, follow previous releases.
1. Run `econ-version --[patch|minor|major]`
1. Push changes and git tag
1. create opam release (`opam publish`)
