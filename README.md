# Guardian

Generic framework for roles and permissions to be used in our projects

## Limitations and Notes

- Supported Database: Implementation with MariaDb (Sqlite only for testing)
- Context (`ctx`): Allows to have multiple database pools (See [next section](#setup-with-mariadb-backend-multipools))

## Setup with MariaDB backend (MultiPools)

```ocaml
  let open Guardian_backend.Pools in
  let module MariaConfig = struct
    include DefaultConfig

    let database =
      MultiPools
        [ "pool-one", "mariadb://root@database:3306/dev"
        ; "pool-two", "mariadb://root@database:3306/test"
        ]
    ;;
  end
  in
  let module MariaDb = Guardian_backend.MariaDb.Make (Role) (Make (MariaConfig))
  let%lwt () = Lwt_list.iter (fun pool -> MariaDb.migrate ~ctx:["pool", pool] ()) ["pool-one"; "pool-two"]
  (** NOTE: To integrate migrations into your applications migration state see
      e.g. function 'MariaDB.find_migrations *)
```

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
    let* (_: [> `User ] MariaDb.authorizable) = User.to_authorizable ?ctx thomas in
    let* (_: [> `User ] MariaDb.authorizable) = User.to_authorizable ?ctx mike in
    let* (_: [> `User ] MariaDb.authorizable_target) = UserTarget.to_authorizable ?ctx thomas in
    let* (_: [> `User ] MariaDb.authorizable_target) = UserTarget.to_authorizable ?ctx mike in
    let* (_: [> `Article ] MariaDb.authorizable_target) = Article.to_authorizable ?ctx thomas_article in
    let* (_: [> `Article ] MariaDb.authorizable_target) = Article.to_authorizable ?ctx mike_article in
    let* () = MariaDb.Actor.save_rule ?ctx example_role in
    Lwt.return_unit

  (* let mike Update the title of thomas article -> returns a (Article.t, string) Lwt_result.t  *)
  let update_title = Article.update_title ?ctx mike thomas_article "Updated Title"
```

## Development

A guide how to setup the project with devcontainers can be found [here](./.devcontainer/README.md).

### Commands

Most used commands can be found in the following list. For the full list of commands, checkout the `Makefile`.

- `make build` - to build the project
- `make build-watch` - to build and watch the project
- `make test` - to run all tests. This requires a running MariaDB instance (sqlite will be created).

### Release to production

In order to deploy to production:

1. edit `dune-project` and update version `(version 0.0.0)`
1. build the project `dune build` or edit `pool.opam` and update version `version: "0.0.0"`
1. commit
1. tag commit and push
1. create github and opam release
