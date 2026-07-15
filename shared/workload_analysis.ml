open! Lunar_jsont
open Data_repr
open! Rich
open Normal

(** Theoretical targets *)
let total_quests_time quests =
  (* We don't count manually assigned quests as to not biased the proportions
     since manually assigned volunteers will not take part in other quests. *)
  Quests.fold quests ~init:0 ~f:(fun acc q ->
      acc
      + Quest.weighted_duration ~skip_manually_assigned:true ~unit:`Minutes q)
  |> Duration.from_minutes

let theoretical_load ~of_:(volunteer : Volunteer.t) ~on:date day_quests =
  (* TODO Maybe check other factors ? Pro rata of arrival time ? *)
  match (volunteer.initial.arrival, volunteer.initial.departure) with
  | Some arrival, _ when Date.(date < Zoned_datetime.local_date arrival) ->
      Duration.zero
  | _, Some departure when Date.(Zoned_datetime.local_date departure < date) ->
      Duration.zero
  | _ ->
      let theory = volunteer.initial.daily_workload in
      let manually_assigned =
        Quests.fold day_quests ~init:Duration.zero ~f:(fun acc q ->
            if
              (not (Quest.is_free q))
              && Quest.is_manually_assigned_to volunteer q
            then Duration.(acc + q.slot.duration)
            else acc)
      in
      Duration.max theory manually_assigned

let total_theoretical_load volunteers ~on day_quests =
  Volunteers.fold volunteers ~init:0 ~f:(fun acc v ->
      (* Don't count the load of manually assigned volunteers *)
      if v.initial.manually_assigned then acc
      else acc + (Duration.to_minutes @@ theoretical_load ~of_:v ~on day_quests))

let adjusted_load_minutes ?(unit = `Minutes) volunteers volunteer day day_quests
    =
  let volunteer_theoretical_load =
    (* Manually assigned volunteers are not adjusted *)
    theoretical_load ~of_:volunteer ~on:day day_quests |> Duration.to_minutes
  in
  if volunteer.initial.manually_assigned then volunteer_theoretical_load
  else
    (* Available time on that day. Not counting manually assinge volunteers *)
    let total_theoretical_load =
      total_theoretical_load volunteers ~on:day day_quests
    in
    (* Quest time, not counting quests assigned to manually assigned volunteers *)
    let quests_time = total_quests_time day_quests |> Duration.to_minutes in
    let adjustement_coef =
      Float.(of_int volunteer_theoretical_load / of_int total_theoretical_load)
    in
    let adjusted_m = Float.(adjustement_coef * of_int quests_time) in
    let adjusted = Quest.minutes_conv ~unit adjusted_m in
    Logs.debug (fun m ->
        m "%s on %s load: %i / %i = %f   ... * %i = %f" volunteer.name
          (Date.to_string day) volunteer_theoretical_load total_theoretical_load
          adjustement_coef quests_time adjusted);
    Float.to_int adjusted
