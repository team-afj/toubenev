open Vif

let index req _server () =
  Response.with_file ~compression:`DEFLATE req
    (Fpath.v "./docs/grist/index.html")

let js req _server () =
  Response.with_file ~compression:`DEFLATE req
    (Fpath.v "./docs/grist/index.bc.js")

let handle_put_data (req : (Vif.Type.json, Grist_import.data) Request.t) _server
    () =
  Logs.debug (fun m -> m "P0UET");
  Logs.debug (fun m -> m "HANDLE PUT");
  let open Response.Syntax in
  let r =
    let open Result in
    Logs.debug (fun m -> m "BEFORE");
    let+ v = Request.of_json req in
    Logs.debug (fun m -> m "AFTER");
    let _, planning = Grist_import.to_planning v in
    let status = Api.Planning.solve planning in
    (* let+ s =
      Jsont_bytesrw.encode_string Types.Planning.jsont planning
      |> Rresult.R.(map_error msg)
    in
    Logs.debug (fun m -> m "%s" s); *)
    status
  in
  let status =
    match r with
    | Error (`Msg err) ->
        let () = Logs.err (fun m -> m "%s" err) in
        {
          Data_repr.Api.dummy_answer with
          diagnostics = [ (Error, Format.asprintf "ERROR %s" err) ];
          date = now ();
        }
    | Ok s -> s
  in
  let* () = Api.Cors.allow_origin () in
  let* () = Response.with_json req Data_repr.Api.answer_jsont status in
  Response.respond `OK

let or_error = function
  | Ok response -> response
  | Error (`Msg msg) ->
      let () = Logs.err (fun m -> m "%s" msg) in
      let open Response.Syntax in
      let* () = Response.empty in
      Response.respond `Internal_server_error

let handle_optimize (req : (Vif.Type.json, Grist_import.data) Request.t) server
    () =
  let open Result in
  or_error
  @@
  let ortools = Server.device Ortools_device.v server in
  let+ v = Request.of_json req in
  let _, planning = Grist_import.to_planning v in
  let handle = Ortools_device.new_optim ortools planning in
  let open Response.Syntax in
  let* () = Api.Cors.allow_origin () in
  let* () = Response.with_text req handle in
  Response.respond `OK
