module type Sig = Sig.Uuid

module UuidBase = struct
  include Uuidm

  let to_yojson t = `String (to_string t)

  let of_yojson = function
    | `String s ->
      of_string s |> CCOption.to_result (Format.asprintf "Invalid UUID: %s" s)
    | _ -> CCResult.Error "Invalid argument"
  ;;

  let of_string_exn s =
    of_string s
    |> CCOption.get_exn_or (Format.asprintf "'%10s' is not a valid uuid" s)
  ;;

  let create () = v `V4
end

module Actor = struct
  include UuidBase
end

module Target = struct
  include UuidBase
end
