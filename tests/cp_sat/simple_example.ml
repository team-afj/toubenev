open Lunar
open Types

let example_planning : Planning.t =
  let info =
    let start_date =
      Date.make ~year:2026 ~month:May ~day:26 () |> Result.get_ok
    in
    let end_date =
      Date.make ~year:2026 ~month:May ~day:30 () |> Result.get_ok
    in
    { Event_infos.name = "Event 1"; kind = Finite { start_date; end_date } }
  in
  let p1 = Place.make ~slug:"🍺" ~name:"Bar" () in
  let places = CCRAL.of_list [ p1 ] in
  let t1 =
    Task_type.make ~slug:"🍻" ~name:"Service bar" ~specialist_only:false
      ~divisible:true ()
  in
  let task_types = CCRAL.of_list [ t1 ] in
  let volunteers =
    let daily_workload = Duration.from_hours 4 in
    let v1 = Volunteer.make ~daily_workload ~name:"V1" () in
    CCRAL.of_list [ v1 ]
  in
  let quests =
    let slot =
      let start = Time.make ~hour:8 ~min:0 ~sec:0 () |> Result.get_ok in
      { Time_slot.recurrence = Daily; start; duration = Duration.from_hours 2 }
    in
    let q1 =
      Quest.make ~name:"Bar 1" ~task_type:t1 ~place:p1 ~slot
        ~required_volunteers:1 ()
    in
    CCRAL.of_list [ q1 ]
  in
  { info; places; task_types; volunteers; quests }

let model = Cp_model.Model.make example_planning
let response = Ortools_solvers.Sat.solve model

let () =
  let open Ortools.Sat.Response in
  Format.printf "Status %s\n" (string_of_status response.status);
  Format.(printf "%a\n" (pp_print_array pp_print_int) response.solution);
  Format.printf "%s\n" response.solution_info
