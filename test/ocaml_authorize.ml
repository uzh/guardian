module UserStore = struct
  let store =
    [ ("Aron", ["Admin"])
    ; ("Chris", [""])
    ]
  let getRoles user =
    List.assoc user store
end

module User = struct
  type t = string

  let toEntity t =
    Ocaml_authorize.Entity.make ~roles:("User" :: UserStore.getRoles t) ~repr:t ()

  let can =
    Ocaml_authorize.Authorizer.makeChecker
      [ ("Admin", [`Create; `Read; `Update; `Delete]) ]
end

(* ensure that the `User` module conforms to the `AuthorizableEntity` module type. *)
let _ = (module User : Ocaml_authorize.Authorizer.AuthorizableEntity)

module Article = struct
  (** pretend that all these fields aren't publically visible *)
  type t =
    { mutable title: string
    ; mutable content: string
    ; mutable author: User.t
    } [@@deriving make]

  let toEntity t =
    Ocaml_authorize.Entity.make ~roles:["Article"] ~owner:(User.toEntity t.author) ()

  let can =
    Ocaml_authorize.Authorizer.makeChecker
      [ ("Admin", [`Create; `Read; `Update; `Delete]) ]

  let updateTitle actor (t: t) newTitle =
    if can actor `Update (toEntity t)
    then let _ = t.title <- newTitle in Ok t
    else Error "Insufficient access"
end

let _ = (module Article : Ocaml_authorize.Authorizer.AuthorizableEntity)

let () =
  let chris = "Chris" in
  let aron = "Aron" in
  let chrisArticle = Article.make ~title:"Foo" ~content:"Bar" ~author:chris in
  let aronArticle = Article.make ~title:"Fizz" ~content:"Buzz" ~author:aron in
  let () =
    Alcotest.(check bool)
      "Chris can update an article owned by Chris."
      (Result.is_ok
         (Article.updateTitle (User.toEntity chris) chrisArticle "Updated Title"))
      true
  in
  let () =
    Alcotest.(check bool)
      "Aron (admin) can update an article Chris owns"
      (Result.is_ok
         (Article.updateTitle (User.toEntity aron) chrisArticle "Updated Title"))
      true
  in
  let () =
    Alcotest.(check bool)
      "Chris cannot update an article Aron owns"
      (Result.is_error
         (Article.updateTitle (User.toEntity chris) aronArticle "Updated Title"))
      true
  in
  let () =
    Alcotest.(check bool)
      "Article can update itself."
      (Result.is_ok
         (Article.updateTitle (Article.toEntity chrisArticle) chrisArticle "Updated Title"))
      true
  in
  let () =
    Alcotest.(check bool)
      "Article cannot update another article."
      (Result.is_error
         (Article.updateTitle (Article.toEntity chrisArticle) aronArticle "Updated Title"))
      true
  in
  ()
