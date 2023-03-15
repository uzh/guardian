module Actor : sig
  include
    Guardian.RoleSig
      with type t =
        [ `User
        | `Admin
        | `Hacker
        | `Editor of Guardian.Contract.Uuid.Target.t
        ]
end

module Target : sig
  include
    Guardian.RoleSig
      with type t =
        [ `User
        | `Article
        | `Post
        ]
end
