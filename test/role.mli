include Ocaml_authorize.Role_s
  with type t =
    [ `User
    | `Admin
    | `Article
    | `Hacker
    ]