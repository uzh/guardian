let store =
  [ ("Aron", ["Admin"])
  ; ("Chris", [""])
  ]
let get_roles user =
  List.assoc user store