module Actor : sig
  include
    Guardian.RoleSig
    with type t =
      [ `Admin
      | `Hacker
      | `User
      ]
end

module Role : sig
  include
    Guardian.RoleSig
    with type t =
      [ `Admin
      | `Author
      | `Editor
      | `Reader
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
