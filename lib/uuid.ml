include Uuidm

let of_string_exn s =
  match of_string s with
  | Some x -> x
  | None -> raise(Failure(Printf.sprintf "'%10s' is not a valid uuid" s))