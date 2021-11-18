let app_style = `Cyan
let err_style = `Red
let warn_style = `Yellow
let info_style = `Blue
let debug_style = `Green
let source_style = `Magenta

let pp_header ~pp_h ppf (l, h) =
  match l with
  | Logs.App ->
    (match h with
     | None -> ()
     | Some h -> Fmt.pf ppf "[%a] " Fmt.(styled app_style string) h)
  | Logs.Error ->
    pp_h
      ppf
      err_style
      (match h with
       | None -> "ERROR"
       | Some h -> h)
  | Logs.Warning ->
    pp_h
      ppf
      warn_style
      (match h with
       | None -> "WARNING"
       | Some h -> h)
  | Logs.Info ->
    pp_h
      ppf
      info_style
      (match h with
       | None -> "INFO"
       | Some h -> h)
  | Logs.Debug ->
    pp_h
      ppf
      debug_style
      (match h with
       | None -> "DEBUG"
       | Some h -> h)

let pp_source = Fmt.(styled source_style string)

let pp_exec_header src =
  let pp_h ppf style h =
    let src = Logs.Src.name src in
    let now = Ptime.of_float_s (Unix.gettimeofday()) |> Option.get |> Ptime.to_rfc3339 in
    Fmt.pf ppf "%s [%a] [%a]: " now Fmt.(styled style string) h pp_source src
  in
  pp_header ~pp_h

let format_reporter
    ?(pp_header = pp_exec_header)
    ?(app = Format.std_formatter)
    ?(dst = Format.err_formatter)
    ()
  =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    msgf
    @@ fun ?header ?tags:_ fmt ->
    let ppf = if level = Logs.App then app else dst in
    Format.kfprintf
      k
      ppf
      ("%a@[" ^^ fmt ^^ "@]@.")
      (pp_header src)
      (level, header)
  in
  { Logs.report }

let cli_reporter ?(pp_header = pp_exec_header) ?app ?dst () =
  Fmt_tty.setup_std_outputs ();
  format_reporter ~pp_header ?app ?dst ()

type event_status =
  | Approved
  | Denied
[@@deriving show]

type ('a, 'b) authorization_event =
  { actor: 'a Authorizable.t
  ; target: 'b Authorizable.t
  ; time: Ptime.t
  ; action: Action.t
  ; status: event_status
  } [@@deriving show,make]

let log (evt : ('a, 'b) authorization_event) =
  let ev = show_authorization_event (fun _ _ -> ()) (fun _ _ -> ()) evt in
  Logs.info (fun m -> m "%s" ev)

let log_txt s =
  let () = print_endline ("Logging: " ^ s) in
  Logs.info (fun m -> m "%s" s)
