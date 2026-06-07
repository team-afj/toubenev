open! Lunar_jsont
open Data_repr
open! Rich
open Normal

(** Utilites *)
let total_quest_time (q : Quest.t) =
  Duration.to_minutes q.slot.duration * q.initial.required_volunteers

let total_quests_time =
  Quests.fold ~init:0 ~f:(fun acc q -> acc + total_quest_time q)

(** Theoretical targets *)

let theoretical_load ~of_:volunteer ~on:_ =
  (* TODO IMPORTANT CHECK ARRIVAL DEPARTURE AND OTHER FACTORS *)
  Duration.to_seconds volunteer.Volunteer.initial.daily_workload

let total_theoretical_load volunteers ~on =
  Volunteers.fold volunteers ~init:0 ~f:(fun acc v ->
      acc + theoretical_load ~of_:v ~on)

let theoretical_coef volunteers ~of_ ~on =
  Float.(
    of_int (theoretical_load ~of_ ~on)
    / of_int (total_theoretical_load volunteers ~on))

let adjusted_load_minutes volunteers volunteer day day_quests =
  let quests_time = total_quests_time day_quests in
  let adjustement_coef = theoretical_coef volunteers ~of_:volunteer ~on:day in
  Float.(to_int (adjustement_coef * of_int quests_time))
