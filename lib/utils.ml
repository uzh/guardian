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

let failwith_invalid_role ?(msg_prefix = "Invalid role") =
  let open CCFun in
  [%show: string * string list]
  %> Format.asprintf "%s: %s" msg_prefix
  %> failwith
;;

module Dynparam = struct
  type t = Pack : 'a Caqti_type.t * 'a -> t

  let empty = Pack (Caqti_type.unit, ())
  let add t x (Pack (t', x')) = Pack (Caqti_type.tup2 t' t, (x', x))
end
