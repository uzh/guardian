# Guardian

Generic framework for roles and permissions to be used in our projects

## Limitations and Notes

- Supported Database: Implementation with MariaDb
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
  let module MariaDb = Guardian_backend.MariaDb.Make (Roles) (Make (MariaConfig))
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

### Integrate within a query (MariaDB)

Requirement: Run `MariaDb.start` when the executable is started.

The `start` function defines needed MariaDB functions for guardian. They can be integrated within your queries as well.
It creates a `guardianValidate<Role>Uuid(...)` function for all roles, it takes care of all registered Relations within guardian.

The following example shows how to check for a specific Post:

```ocaml
(** [guardianValidatePostUuid]

    Input arguments:
      - actor_uuid (binary 16)
      - action (enum: 'create','read','update','delete','manage')
      - target_uuid (binary 16, usually within the query)

    Outputs a boolean
*)
let find_accessible kind =
  {sql|
    SELECT *
    FROM posts
    GROUP BY posts.uuid
    HAVING guardianValidatePostUuid(guardianEncodeUuid(?), ?, posts.uuid)
  |sql}
  |> Caqti_type.(t2 UuidActor.t Action.t ->* Post.t)
```

## Development

A guide how to setup the project with devcontainers can be found [here](./.devcontainer/README.md).

### Commands

Most used commands can be found in the following list. For the full list of commands, checkout the `Makefile`.

- `make build` - to build the project
- `make build-watch` - to build and watch the project
- `make test` - to run all tests. This requires a running MariaDB instance.

## 🚀 Release new version

1. Update `CHANGELOG.md` and document changes made. Ensure the version to be releases has a header matching the version, follow previous releases.
1. Edit the file `dune-project` and update version `(version 0.0.0)`.
1. Build the project `dune build` __OR__ edit the file `pool.opam` and update version `version: "0.0.0"`
1. Commit your changes.
1. Tag the commit and push changes and git tag
1. create opam release (`opam-publish`)
