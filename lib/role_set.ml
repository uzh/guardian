include Set.Make(Role)

let singleton elt =
  singleton (String.lowercase_ascii elt)

let mem elt t =
  mem (String.lowercase_ascii elt) t

let add elt t =
  add (String.lowercase_ascii elt) t

let to_yojson t : Yojson.Safe.t =
  `List(List.map (fun s -> `String s) (elements t))

let of_yojson = function
  | `List items ->
    List.fold_left
      (fun acc x ->
         Result.bind
           acc
           (fun acc' ->
              match x with
              | `String s -> Ok (add s acc')
              | _ -> Error "Invalid role."
           )
      )
      (Ok empty)
      items
  | _ ->
    Error "Invalid role set"

let pp fmt t =
  let show = [%show: Role.t list] in
  Format.fprintf fmt "[%s]" (show (elements t))