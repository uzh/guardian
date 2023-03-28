module Actor : sig
  include
    Guardian.RoleSig
      with type t =
        [ `Admin
        | `Editor of Guardian.Contract.Uuid.Target.t
        | `Hacker
        | `Reader of Guardian.Contract.Uuid.Target.t
        | `User
        ]
end

module Target : sig
  include
    Guardian.RoleSig
      with type t =
        [ `Article
        | `Note
        | `Post
        | `User
        ]
end
