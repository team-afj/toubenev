open! Lunar_jsont
open Data_repr
open! Rich
open Normal

(** Theoretical targets *)
let total_quests_time quests =
  (* We don't count manually assigned quests as to not bias the proportions
     since manually assigned volunteers will not take part in other quests. *)
  Quests.fold quests ~init:0 ~f:(fun acc q ->
      acc
      + Quest.weighted_duration ~skip_manually_assigned:true ~unit:`Minutes q)
  |> Duration.from_minutes

(* TODO this should be memoized *)
let theoretical_load static_checks ~of_:(volunteer : Volunteer.t) ~on:date
    day_quests =
  (* TODO Maybe check other factors ? Pro rata of arrival time ? *)
  match (volunteer.initial.arrival, volunteer.initial.departure) with
  | Some arrival, _ when Date.(date < Zoned_datetime.local_date arrival) ->
      `Fixed Duration.zero
  | _, Some departure when Date.(Zoned_datetime.local_date departure < date) ->
      `Fixed Duration.zero
  | _ ->
      let debug = String.equal "Hugo" volunteer.name in
      let () =
        if debug then
          Logs.debug (fun m ->
              m "%s on %s" volunteer.name (Date.to_intl_long_string `Fr date))
      in
      let available_hours =
        let key =
          Date.to_duration date |> Duration.to_int64 |> Int64.to_string
        in
        static_checks.Static_analysis.max_doable ~key volunteer day_quests
      in
      let theory =
        Duration.min volunteer.initial.daily_workload available_hours
      in
      let () =
        if debug then
          Logs.debug (fun m ->
              m "%s available: %s; theory: %s" volunteer.name
                (Duration.to_string available_hours)
                (Duration.to_string volunteer.initial.daily_workload))
      in
      let manually_assigned =
        Quests.fold day_quests ~init:Duration.zero ~f:(fun acc q ->
            if
              (not (Quest.is_free q))
              && Static_analysis.v_is_manually_assigned_to_q volunteer q
            then Duration.(acc + q.slot.duration)
            else acc)
      in
      let () =
        if debug then
          Logs.debug (fun m ->
              m "%s manual: %s" volunteer.name
                (Duration.to_string manually_assigned))
      in
      let final = Duration.max theory manually_assigned in
      if Duration.(final >= available_hours) then `Fixed final
      else `Flexible final

let total_theoretical_load static_checks volunteers ~on day_quests =
  Volunteers.fold volunteers ~init:0 ~f:(fun acc v ->
      (* Don't count the load of manually assigned volunteers *)
      (* TODO All "Fixed" volunteers should be ignored *)
      if v.initial.manually_assigned then acc
      else
        let v_load =
          match theoretical_load static_checks ~of_:v ~on day_quests with
          | `Fixed load | `Flexible load -> load
        in
        acc + Duration.to_minutes v_load)

let adjusted_load_minutes static_checks ?(unit = `Minutes) volunteers volunteer
    day day_quests =
  let volunteer_theoretical_load =
    (* Manually assigned volunteers are not adjusted *)
    theoretical_load static_checks ~of_:volunteer ~on:day day_quests
  in
  (* TODO it's not the same to adjust up than down ! Some volunteers might not
     be completely fixed, but not adjustable UP. *)
  match (volunteer.initial.manually_assigned, volunteer_theoretical_load) with
  | true, (`Fixed load | `Flexible load) | false, `Fixed load ->
      Duration.to_minutes load |> Float.of_int |> Quest.minutes_conv ~unit
      |> Float.round |> Float.to_int
  | false, `Flexible load ->
      let volunteer_theoretical_load = Duration.to_minutes load in
      (* Available time on that day. Not counting manually assigned volunteers *)
      let total_theoretical_load =
        total_theoretical_load static_checks volunteers ~on:day day_quests
      in
      (* Quest time, not counting quests assigned to manually assigned volunteers *)
      let quests_time = total_quests_time day_quests |> Duration.to_minutes in
      let adjustement_coef =
        Float.(
          of_int volunteer_theoretical_load / of_int total_theoretical_load)
      in
      let adjusted_m = Float.(adjustement_coef * of_int quests_time) in
      let adjusted = Quest.minutes_conv ~unit adjusted_m in
      (* Logs.debug (fun m ->
          m "%s on %s load: %i / %i = %f   ... * %i = %f" volunteer.name
            (Date.to_string day) volunteer_theoretical_load
            total_theoretical_load adjustement_coef quests_time adjusted); *)
      Float.to_int adjusted
