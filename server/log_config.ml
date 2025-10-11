(* This comes from vif *)

let app_style = `Cyan
let err_style = `Red
let warn_style = `Yellow
let info_style = `Blue
let debug_style = `Green

let pp_header ~pp_h ppf (l, h) =
  match l with
  | Logs.Error ->
      let h = Option.value ~default:"ERROR" h in
      pp_h ppf err_style h
  | Logs.Warning ->
      let h = Option.value ~default:"WARN" h in
      pp_h ppf warn_style h
  | Logs.Info ->
      let h = Option.value ~default:"INFO" h in
      pp_h ppf info_style h
  | Logs.Debug ->
      let h = Option.value ~default:"DEBUG" h in
      pp_h ppf debug_style h
  | Logs.App ->
      Fun.flip Option.iter h @@ fun h ->
      Fmt.pf ppf "[%a] " Fmt.(styled app_style (fmt "%10s")) h

let pp_header =
  let pp_h ppf style h = Fmt.pf ppf "[%a]" Fmt.(styled style (fmt "%10s")) h in
  pp_header ~pp_h

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("[%02d]%a[%a]: @[<hov>" ^^ fmt ^^ "@]\n%!")
        (Stdlib.Domain.self () :> int)
        pp_header (level, header)
        Fmt.(styled `Magenta (fmt "%20s"))
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }
