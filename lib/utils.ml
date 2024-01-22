open CCFun

let hide_typ f (_ : 'a) = Format.pp_print_string f ""

(** turn a single argument function returning a [result] into one that raises a
    [Failure] instead *)
let with_exn ?ctx f name arg =
  match%lwt f ?ctx arg with
  | Ok x -> Lwt.return x
  | Error s -> failwith @@ Format.asprintf "%s failed: %s" name s
;;

let decompose_variant_string s =
  let open CCString in
  let s = trim s in
  let fmt = format_of_string "`%s (%s@)" in
  try
    Scanf.sscanf s fmt (fun name params ->
      lowercase_ascii name, CCList.map trim (split_on_char ',' params))
  with
  | End_of_file ->
    let fmt = format_of_string "`%s" in
    Scanf.sscanf s fmt (fun name -> lowercase_ascii name, [])
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
