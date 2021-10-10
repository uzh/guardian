module User_store = struct
  let store =
    [ ("Aron", ["Admin"])
    ; ("Chris", [""])
    ]
  let get_roles user =
    List.assoc user store
end

module User = struct
  type t = string

  let to_entity t =
    let open Ocaml_authorize in
    Entity.make
      ~roles:(Role_set.of_list ("User" :: User_store.get_roles t))
      ~repr:t
      ()

  let can =
    Ocaml_authorize.Authorizer.make_checker
      [ ("Admin", [`Create; `Read; `Update; `Delete]) ]
end

(* ensure that the `User` module conforms to the `Authorizable_entity` module type. *)
let _ = (module User : Ocaml_authorize.Authorizer.Authorizable_entity)

module Article = struct
  (** pretend that all these fields aren't publically visible *)
  type t =
    { mutable title: string
    ; mutable content: string
    ; mutable author: User.t
    } [@@deriving make]

  let to_entity t =
    let open Ocaml_authorize in
    Entity.make
      ~roles:Role_set.(add "Article" empty)
      ~owner:(User.to_entity t.author)
      ()

  let can =
    Ocaml_authorize.Authorizer.make_checker
      [ ("Admin", [`Create; `Read; `Update; `Delete]) ]

  let update_title actor (t: t) new_title =
    if can actor `Update (to_entity t)
    then let _ = t.title <- new_title in Ok t
    else Error "Insufficient access"
end

let _ = (module Article : Ocaml_authorize.Authorizer.Authorizable_entity)

let () =
  let chris = "Chris" in
  let aron = "Aron" in
  let chris_article = Article.make ~title:"Foo" ~content:"Bar" ~author:chris in
  let aron_article = Article.make ~title:"Fizz" ~content:"Buzz" ~author:aron in
  let () =
    Alcotest.(check bool)
      "Chris can update an article owned by Chris."
      (Result.is_ok
         (Article.update_title (User.to_entity chris) chris_article "Updated Title"))
      true
  in
  let () =
    Alcotest.(check bool)
      "Aron (admin) can update an article Chris owns"
      (Result.is_ok
         (Article.update_title (User.to_entity aron) chris_article "Updated Title"))
      true
  in
  let () =
    Alcotest.(check bool)
      "Chris cannot update an article Aron owns"
      (Result.is_error
         (Article.update_title (User.to_entity chris) aron_article "Updated Title"))
      true
  in
  let () =
    Alcotest.(check bool)
      "Article can update itself."
      (Result.is_ok
         (Article.update_title (Article.to_entity chris_article) chris_article "Updated Title"))
      true
  in
  let () =
    Alcotest.(check bool)
      "Article cannot update another article."
      (Result.is_error
         (Article.update_title (Article.to_entity chris_article) aron_article "Updated Title"))
      true
  in
  ()
