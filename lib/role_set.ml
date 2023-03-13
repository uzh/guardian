module type S = sig
  include Set.S

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) CCResult.t
  val pp : Format.formatter -> t -> unit
end

module Make (Role : Role.Sig) : S with type elt = Role.t = struct
  include Set.Make (Role)

  let to_yojson t = `List (CCList.map Role.to_yojson (elements t))

  let of_yojson =
    let open CCResult in
    function
    | `List items ->
      CCList.fold_left
        (fun acc x ->
          acc >>= fun acc' -> x |> Role.of_yojson >|= CCFun.flip add acc')
        (Ok empty)
        items
    | _ -> Error "Invalid role set"
  ;;

  let pp fmt t =
    let show = [%show: Role.t list] in
    Format.fprintf fmt "[%s]" (show (elements t))
  ;;
end
