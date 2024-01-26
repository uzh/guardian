let find_request_sql
  sql_select_columns
  table_name
  ?(default_where = None)
  ?(count = false)
  ?(joins = "")
  where_fragment
  =
  let where_fragment =
    CCOption.map_or ~default:where_fragment (fun default_where ->
      [%string
        {sql|
          WHERE %{default_where}
            AND %{CCString.replace ~which:`Left ~by:"" ~sub:"WHERE" where_fragment}
        |sql}])
  in
  let columns =
    if count then "COUNT(*)" else sql_select_columns |> CCString.concat ", "
  in
  Format.asprintf
    {sql|SELECT %s FROM %s %s %s|sql}
    columns
    table_name
    joins
    (where_fragment default_where)
;;
