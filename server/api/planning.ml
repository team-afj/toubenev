(** Run the model without the optimizer to check feasibility *)
let sat_check planning =
  let context = Cp_model.Model.make ~with_assumptions:false planning in
  let parameters =
    Ortools.Sat_parameters.make_sat_parameters ~stop_after_first_solution:true
      ~log_search_progress:false ~num_workers:8l ()
  in
  let time_before = Unix.gettimeofday () in
  let response = Ortools_solvers.Sat.solve ~parameters context.model in
  Logs.debug (fun m ->
      m "Solver took %fs" (Unix.gettimeofday () -. time_before));
  if Equal.poly response.status Ortools.Sat.Response.Infeasible then
    (* If the model is not satisfaiable, we run again with assumptions to
       provide some hints about the conflicting constraints. We do this in a
       second pass because it requires running in single thread mode. *)
    let context = Cp_model.Model.make ~with_assumptions:true planning in
    let parameters =
      Ortools.Sat_parameters.make_sat_parameters ~stop_after_first_solution:true
        ~log_search_progress:true ~log_to_stdout:false ~log_to_response:true
        ~num_workers:1l ()
    in
    (context, Ortools_solvers.Sat.solve ~parameters context.model)
  else (context, response)

let solve planning =
  let context, response = sat_check planning in
  let date = now ~tz:planning.infos.timezone () in
  Cp_model.Context.prepare_answer date context response

open Vif

let optim_stream req handle server () =
  let open Response.Syntax in
  let ortools = Server.device Ortools_device.v server in
  match Ortools_device.get_queue ortools handle with
  | None ->
      let* () = Response.empty in
      Response.respond `Not_found
  | Some queue ->
      let open Response.Syntax in
      let src = Flux.Source.bqueue queue in
      let src =
        Flux.Source.map
          (fun answer ->
            let json =
              Jsont_bytesrw.encode_string Data_repr.Api.answer_jsont answer
              |> Result.get_ok
            in
            Printf.sprintf "data: %s\n\n" json)
          src
      in
      let* () =
        let field = "content-type" in
        Response.add ~field "text/event-stream"
      in
      let* () = Cors.allow_origin () in
      let* () = Response.with_source ~compression:`DEFLATE req src in
      let* () = Response.respond `OK in
      Logs.debug (fun fmt -> fmt "SSE connection closed");
      Ortools_device.cancel ortools handle;
      Response.return ()
