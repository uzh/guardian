module type S = sig
  include Set.S

  val to_yojson : t -> Yojson.Safe.t

  val of_yojson : Yojson.Safe.t -> (t, string) Result.t

  val pp : Format.formatter -> t -> unit
end

module Make(R : Role.S) : (S with type elt = R.t) = struct
  include Set.Make(R)

  let to_yojson t =
    `List(List.map R.to_yojson (elements t))

  let of_yojson = function
    | `List items ->
      List.fold_left
        (fun acc x ->
          Result.bind
            acc
            (fun acc' ->
                match R.of_yojson x with
                | Ok role -> Ok(add role acc')
                | Error _ as err -> err
            )
        )
        (Ok empty)
        items
    | _ ->
      Error "Invalid role set"

  let pp fmt t =
    let show = [%show: R.t list] in
    Format.fprintf fmt "[%s]" (show (elements t))
end