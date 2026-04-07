module type Sig = Sig.Uuid

module UuidBase = struct
  include Uuidm

  (** Returns [uuid] if valid, raises [Invalid_argument] otherwise *)
  let of_string_exn ?pos s =
    of_string ?pos s
    |> CCOption.get_exn_or (Format.asprintf "Invalid UUID: %s" s)
  ;;

  (** Returns [Ok uuid] if valid, [Error msg] otherwise *)
  let of_string_res ?pos s =
    of_string ?pos s
    |> CCOption.to_result (Format.asprintf "Invalid UUID: %s" s)
  ;;

  let to_yojson t = `String (to_string t)

  let of_yojson = function
    | `String s -> of_string_res s
    | _ -> Error "Expected a JSON string for UUID"
  ;;

  let sexp_of_t uuid = Sexplib0.Sexp.Atom (to_string uuid)
  let create = v4_gen (Random.State.make_self_init ())
end

module Actor = struct
  include UuidBase
end

module Target = struct
  include UuidBase
end
