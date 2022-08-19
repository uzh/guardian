include Guardian.Role_s
  with type t =
    [ `User
    | `Admin
    | `Article
    | `Hacker
    | `Editor of Uuidm.t
    ]
