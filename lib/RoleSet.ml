include Set.Make(Role)

let pp t =
  let show = [%show: Role.t list] in
  show (elements t)