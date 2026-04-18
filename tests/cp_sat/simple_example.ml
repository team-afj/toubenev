open Lunar
open Types

let () = Mirage_crypto_rng_unix.use_default ()

let example_planning : Planning.t =
  let options = Options.default in
  let infos =
    let start_date =
      Date.make ~year:2026 ~month:May ~day:26 () |> Result.get_ok
    in
    let end_date =
      Date.make ~year:2026 ~month:May ~day:30 () |> Result.get_ok
    in
    {
      Event_infos.name = "Event 1";
      kind = Finite { start_date; end_date };
      timezone = Timezones.europe_paris;
    }
  in
  let p1 = Place.make ~slug:"🍺" ~name:"Bar" () in
  let places = CCRAL.of_list [ p1 ] in
  let t1 =
    Task_type.make ~slug:"🍻" ~name:"Service bar" ~specialist_only:false
      ~divisible:true ()
  in
  let t2 =
    Task_type.make ~slug:"👋" ~name:"Accueil" ~specialist_only:false
      ~divisible:true ()
  in
  let task_types = CCRAL.of_list [ t1; t2 ] in
  let daily_workload = Duration.from_hours 4 in
  let v1 =
    Volunteer.make ~daily_workload ~name:"V1" ~manually_assigned:true ()
  in
  let v2 = Volunteer.make ~daily_workload ~name:"V2" () in
  let volunteers = CCRAL.of_list [ v1; v2 ] in
  let quests =
    let slot =
      let start = Time.make ~hour:8 ~min:0 ~sec:0 () |> Result.get_ok in
      Time_spec.make Daily start @@ Duration.from_hours 2
    in
    let q1 =
      Quest.make ~name:"Bar" ~task_type:t1 ~place:p1 ~slot
        ~required_volunteers:1 ~assigned_volunteers:(CCRAL.of_list [ v1 ]) ()
    in
    let q2 =
      Quest.make ~name:"Accueil" ~task_type:t2 ~place:p1 ~slot
        ~required_volunteers:1 ()
    in
    CCRAL.of_list [ q1; q2 ]
  in
  { options; infos; places; task_types; volunteers; quests }

let context = Cp_model.Model.make ~with_assumptions:true example_planning

let parameters =
  Ortools.Sat_parameters.make_sat_parameters ~log_search_progress:false ()

let response = Ortools_solvers.Sat.solve ~parameters context.model

let () =
  let open Ortools.Sat.Response in
  let solution = Cp_model.Model.resolve_solution context response.solution in
  Format.printf "\nStatus %s\n" (string_of_status response.status);
  Format.(printf "Solution:@ @[%a@]\n" Cp_model.Model.pp_solution solution);
  Format.printf "Solution info: %s\n" response.solution_info;
  Format.printf "Log: %s\n" response.solve_log
