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
    let+ v = Request.of_json req in
    Logs.debug (fun m -> m "AFTER");
    let planning = Grist_import.to_planning v in
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
        {
          Api.Planning.status = Format.asprintf "ERROR %s" err;
          solution = "";
          sufficient_assumptions_for_infeasibility = "";
          log = "";
        }
    | Ok s -> s
  in
  let* () = Response.with_json req Api.Planning.answer_jsont status in
  Response.respond `OK
