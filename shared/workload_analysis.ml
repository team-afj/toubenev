open! Lunar_jsont
open Data_repr
open! Rich
open Normal

(** Theoretical targets *)
let total_quests_time quests =
  Quests.fold quests ~init:0 ~f:(fun acc q ->
      acc + Quest.weighted_duration ~unit:`Minutes q)
  |> Duration.from_minutes

let theoretical_load ~of_:(volunteer : Volunteer.t) ~on:date =
  (* TODO Maybe check other factors ? Pro rata of arrival time ? *)
  match (volunteer.initial.arrival, volunteer.initial.departure) with
  | Some arrival, _ when Date.(date < Zoned_datetime.local_date arrival) ->
      Duration.zero
  | _, Some departure when Date.(Zoned_datetime.local_date departure < date) ->
      Duration.zero
  | _ -> volunteer.initial.daily_workload

let total_theoretical_load volunteers ~on =
  Volunteers.fold volunteers ~init:0 ~f:(fun acc v ->
      acc + (Duration.to_minutes @@ theoretical_load ~of_:v ~on))

let theoretical_coef ~total ~of_ ~on =
  Float.(
    of_int (Duration.to_minutes @@ theoretical_load ~of_ ~on) / of_int total)

let adjusted_load_minutes ?(unit = `Minutes) volunteers volunteer day day_quests
    =
  let quests_time = total_quests_time day_quests |> Duration.to_minutes in
  let total = total_theoretical_load volunteers ~on:day in
  let adjustement_coef = theoretical_coef ~total ~of_:volunteer ~on:day in
  let adjusted_m = Float.(adjustement_coef * of_int quests_time) in
  let adjusted = Quest.minutes_conv ~unit adjusted_m in
  Float.to_int adjusted
