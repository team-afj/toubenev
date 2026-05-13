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

let solve planning =
  let context, response = sat_check planning in
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
    |> String.concat ~sep:"; "
  in
  Logs.debug (fun m -> m "Status: %s" (string_of_status response.status));
  let date = now ~tz:planning.infos.timezone () in
  {
    Data_repr.Api.status = response.status;
    diagnostics = [];
    solution;
    sufficient_assumptions_for_infeasibility;
    log = response.solve_log;
    date;
  }
