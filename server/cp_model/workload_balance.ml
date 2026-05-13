open Ortools
open! Lunar_jsont
open Data_repr
open! Rich
open Normal

(** Utilites *)
let total_quest_time (q : Quest.t) =
  Duration.to_minutes q.slot.duration * q.initial.required_volunteers

let total_quests_time =
  Quests.fold ~init:0 ~f:(fun acc q -> acc + total_quest_time q)

(** Real assignations *)

(** [time_spent ctx ~by:volunteer ~on:quests] sums the duration of each quest in
    [quests] that is assigned to [volunteer]. *)
let time_spent (ctx : Context.t) ~by:volunteer ~on:quests =
  Sat.LinearExpr.weighted_sum
  @@ Quests.to_list_map quests ~f:(fun q ->
      let assigned = ctx.assignations volunteer q in
      (Duration.to_minutes q.slot.duration, assigned))

(** Theoretical targets *)

let theoretical_load (_ctx : Context.t) ~of_:volunteer ~on:_ =
  (* TODO IMPORTANT CHECK ARRIVAL DEPARTURE AND OTHER FACTORS *)
  Duration.to_seconds volunteer.Volunteer.initial.daily_workload

let total_theoretical_load (ctx : Context.t) ~on =
  Volunteers.fold ctx.vs ~init:0 ~f:(fun acc v ->
      acc + theoretical_load ctx ~of_:v ~on)

let theoretical_coef (ctx : Context.t) ~of_ ~on =
  Float.(
    of_int (theoretical_load ctx ~of_ ~on)
    / of_int (total_theoretical_load ctx ~on))

let adjusted_load (ctx : Context.t) volunteer day day_quests =
  let quests_time = total_quests_time day_quests in
  let adjustement_coef = theoretical_coef ctx ~of_:volunteer ~on:day in
  Float.(to_int (adjustement_coef * of_int quests_time))

(** Diffs *)

let load_diff (ctx : Context.t) volunteer day day_quests =
  let time_spent = time_spent ctx ~by:volunteer ~on:day_quests in
  let adjusted_load = adjusted_load ctx volunteer day day_quests in
  Sat.(time_spent - of_int adjusted_load)

(** Lower and upper bounds *)

let max_daily_load (ctx : Context.t) =
  60
  * Volunteers.fold ctx.vs ~init:0 ~f:(fun max_so_far v ->
      max max_so_far (Duration.to_seconds v.initial.daily_workload))

let bounds (ctx : Context.t) day day_quests =
  let s_date = Date.to_string day in
  let max_daily_load = max_daily_load ctx in
  let lb = -2 * max_daily_load in
  let ub = 2 * max_daily_load in
  let lower_bound =
    Printf.sprintf "diff_lower_bound_day_%s" s_date
    |> Sat.Var.new_int ctx.model ~lb ~ub
  in
  let upper_bound =
    Printf.sprintf "diff_upper_bound_day_%s" s_date
    |> Sat.Var.new_int ctx.model ~lb ~ub
  in
  let () =
    ctx.for_all_volunteers @@ fun v ->
    let diff = load_diff ctx v day day_quests in
    Sat.(add ctx.model (diff <= var upper_bound));
    Sat.(add ctx.model (diff >= var lower_bound))
  in
  Sat.(var upper_bound - var lower_bound)

let sum_of_all_daily_bounds (ctx : Context.t) =
  Date.Map.fold
    (fun day day_quests acc -> bounds ctx day day_quests :: acc)
    ctx.by_day []
  |> Sat.LinearExpr.sum
