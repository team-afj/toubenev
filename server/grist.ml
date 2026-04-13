open Vif

let index req _server () =
  Response.with_file ~compression:`DEFLATE req
    (Fpath.v "./grist/widget/index.html")

let js req _server () =
  Response.with_file ~compression:`DEFLATE req
    (Fpath.v "./grist/widget/index.bc.js")

let handle_put_data (req : (Vif.Type.json, Grist_import.data) Request.t) _server
    () =
  Logs.debug (fun m -> m "P0UET");
  Logs.debug (fun m -> m "HANDLE PUT");
  let open Response.Syntax in
  let r =
    let open Result in
    Logs.debug (fun m -> m "BEFORE");
    let* v = Request.of_json req in
    Logs.debug (fun m -> m "AFTER");
    let+ s =
      Jsont_bytesrw.encode_string Grist_import.data_jsont v
      |> Rresult.R.(map_error msg)
    in
    Logs.debug (fun m -> m "%s" s)
  in
  Result.iter_error (fun (`Msg msg) -> Logs.err (fun m -> m "ARG %s" msg)) r;
  let* () = Response.empty in
  Response.respond `OK
