module UuidBase = struct
  include Uuidm

  let to_yojson t = `String (to_string t)

  let of_yojson = function
    | `String s ->
      of_string s |> CCOption.to_result (Format.asprintf "Invalid UUID: %s" s)
    | _ -> raise (Invalid_argument "")
  ;;

  let of_string_exn s =
    match of_string s with
    | Some x -> x
    | None -> failwith @@ Format.asprintf "'%10s' is not a valid uuid" s
  ;;

  let create () = v `V4
end

module Actor = struct
  include UuidBase
end

module Target = struct
  include UuidBase
end

let actor_of_target = CCFun.(Target.to_string %> Actor.of_string_exn)
let target_of_actor = CCFun.(Actor.to_string %> Target.of_string_exn)
