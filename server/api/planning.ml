let solve planning =
  let context = Cp_model.Model.make planning in
  let parameters =
    Ortools.Sat_parameters.make_sat_parameters ~log_search_progress:false ()
  in
  let response = Ortools_solvers.Sat.solve ~parameters context.model in
  let open Ortools.Sat.Response in
  let solution = Cp_model.Model.resolve_solution context response.solution in
  Logs.debug (fun m -> m "Status: %s" (string_of_status response.status));
  Logs.debug (fun m ->
      m "Solution:@ @[%a@]\n" Cp_model.Model.pp_solution solution);
  string_of_status response.status
