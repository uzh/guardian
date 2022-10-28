# Guardian

Generic framework for roles and permissions to be used in our projects


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
  let module Maria = Guardian_backend.MariaDb.Make (Role) (Make (MariaConfig))
  let%lwt () = Lwt_list.iter (fun pool -> Maria.migrate ~ctx:["pool", pool] ()) ["pool-one"; "pool-two"]
  (** NOTE: To integrate migrations into your applications migration state see
      e.g. function 'Maria.find_migrations *)
```

TODO

- [ ] Log access requests
- [ ] Lwt/Async support
