open CCFun

let hide_typ f (_ : 'a) = Format.pp_print_string f ""

(** turn a single argument function returning a [result] into one that raises a
    [Failure] instead *)
let with_exn ?ctx f name arg =
  match%lwt f ?ctx arg with
  | Ok x -> Lwt.return x
  | Error s -> failwith @@ Format.asprintf "%s failed: %s" name s
;;

let decompose_variant_string input =
  let open CCString in
  let try_scan fmt f =
    match Scanf.sscanf (trim input) fmt f with
    | result -> Some result
    | exception (End_of_file | Failure _ | Invalid_argument _ | _) -> None
  in
  [ (fun () ->
      try_scan "`%[^(](%[^)])" (fun name params ->
        lowercase_ascii (trim name), CCList.map trim (split_on_char ',' params)))
  ; (fun () -> try_scan "`%s" (fun name -> lowercase_ascii name, []))
  ]
  |> CCList.find_map (fun f -> f ())
;;

let decompose_variant_string_exn input =
  decompose_variant_string input
  |> CCOption.get_exn_or (Format.asprintf "Invalid variant string: %s" input)
;;

let invalid_role ?(msg_prefix = "Invalid role") =
  [%show: string * string list] %> Format.asprintf "%s: %s" msg_prefix
;;

let failwith_invalid_role ?msg_prefix = invalid_role ?msg_prefix %> failwith

module Dynparam = struct
  type t = Pack : 'a Caqti_type.t * 'a -> t

  let empty = Pack (Caqti_type.unit, ())
  let add t x (Pack (t', x')) = Pack (Caqti_type.t2 t' t, (x', x))
end

let deny_message_uuid actor_uuid permission target_uuid =
  Format.asprintf
    "Actor '%s': Permission ('%s') denied for the target '%s'"
    ([%show: Uuid.Actor.t] actor_uuid)
    ([%show: Permission.t] permission)
    ([%show: Uuid.Target.t] target_uuid)
;;

let deny_message_for_str_target actor_uuid permission str_target =
  Format.asprintf
    "Actor '%s': Permission ('%s') denied for the target '%s'"
    ([%show: Uuid.Actor.t] actor_uuid)
    ([%show: Permission.t] permission)
    str_target
;;

let deny_message_validation_set actor_uuid str_target =
  Format.asprintf
    "Actor '%s': Permission denied for the set '%s'"
    ([%show: Uuid.Actor.t] actor_uuid)
    str_target
;;
