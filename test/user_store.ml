let store =
  [ ("Aron", ["Admin"])
  ; ("Chris", [""])
  ]
let get_roles (name, _uuid) =
  List.assoc name store