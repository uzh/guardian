include Set.Make(Role)

let pp fmt t =
  let show = [%show: Role.t list] in
  Format.fprintf fmt "[%s]" (show (elements t))