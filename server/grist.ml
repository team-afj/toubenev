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
    let planning = Grist_import.to_planning v in
    let status = Api.Planning.solve planning in
    Logs.debug (fun m -> m "%s" s);
    status
  in
  let status =
    match r with Error (`Msg err) -> Format.sprintf "ERROR %s" err | Ok s -> s
  in
  let* () = Response.with_string req status in
  Response.respond `OK
