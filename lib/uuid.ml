include Uuidm

let to_yojson t =
  `String (to_string t)

let of_yojson = function
  | `String s ->
    of_string s
    |> begin function
      | Some x -> Ok x
      | None -> Error("Invalid UUID: " ^ s)
    end
  | _ -> raise (Invalid_argument "")

let of_string_exn s =
  match of_string s with
  | Some x -> x
  | None -> raise(Failure(Printf.sprintf "'%10s' is not a valid uuid" s))
