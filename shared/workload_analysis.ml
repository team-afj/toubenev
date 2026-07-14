open! Lunar_jsont
open Data_repr
open! Rich
open Normal

(** Theoretical targets *)
let total_quests_time quests =
  Quests.fold quests ~init:0 ~f:(fun acc q ->
      acc + Quest.weighted_duration ~unit:`Minutes q)
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
      acc + (Duration.to_minutes @@ theoretical_load ~of_:v ~on day_quests))

let theoretical_coef ~total ~of_ ~on day_quests =
  Float.(
    of_int (Duration.to_minutes @@ theoretical_load ~of_ ~on day_quests)
    / of_int total)

let adjusted_load_minutes ?(unit = `Minutes) volunteers volunteer day day_quests
    =
  let quests_time = total_quests_time day_quests |> Duration.to_minutes in
  let total = total_theoretical_load volunteers ~on:day day_quests in
  let adjustement_coef =
    theoretical_coef ~total ~of_:volunteer ~on:day day_quests
  in
  let adjusted_m = Float.(adjustement_coef * of_int quests_time) in
  let adjusted = Quest.minutes_conv ~unit adjusted_m in
  Float.to_int adjusted
