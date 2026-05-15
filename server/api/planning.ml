open Data_repr
open Normal

(** Run the model without the optimizer to check feasibility *)
let sat_check planning =
  let context = Cp_model.Model.make ~with_assumptions:false planning in
  let parameters =
    Ortools.Sat_parameters.make_sat_parameters ~stop_after_first_solution:true
      ~log_search_progress:false ~num_workers:8l ()
  in
  let response = Ortools_solvers.Sat.solve ~parameters context.model in
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

let prepare_answer date context (response : Ortools.Sat.Response.t) =
  let open Ortools.Sat.Response in
  let solution =
    Cp_model.Model.resolve_assignations context response.solution
  in
  let solution =
    Quest.Map.to_list solution
    |> List.map ~f:(fun (quest, volunteers) ->
        let volunteers =
          Volunteers.to_list_map ~f:(fun v -> v.initial) volunteers
        in
        { Data_repr.Api.quest; volunteers })
  in
  let sufficient_assumptions_for_infeasibility =
    List.map
      ~f:(fun v -> Ortools.Sat.Var.to_string v)
      response.sufficient_assumptions_for_infeasibility
  in
  {
    Data_repr.Api.status = response.status;
    diagnostics = [];
    solution;
    sufficient_assumptions_for_infeasibility;
    log = response.solve_log;
    date;
  }

let solve planning =
  let context, response = sat_check planning in
  let date = now ~tz:planning.infos.timezone () in
  prepare_answer date context response

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
          (fun response ->
            let txt =
              Printf.sprintf "data: ping %f\n\n"
                response.Ortools.Sat.Response.best_objective_bound
            in
            Format.eprintf "SSE %s\n%!" txt;
            txt)
          src
      in
      let* () =
        let field = "content-type" in
        Response.add ~field "text/event-stream"
      in
      let* () = Cors.allow_origin () in
      let* () = Response.with_source req src in
      let* () = Response.respond `OK in
      Logs.debug (fun fmt -> fmt "SSE connection closed");
      Ortools_device.cancel ortools handle;
      Response.return ()
