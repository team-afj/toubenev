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

(** [daily_time_spent (ctx : Context.t) ~by:volunteer] returns the time spent
    everyday on quests assigned to [volunteer]. *)
let daily_time_spent (ctx : Context.t) ~by =
  Date.Map.map (fun quests -> time_spent ctx ~by ~on:quests) ctx.by_day

let daily_volunteers_time_spent (ctx : Context.t) =
  Volunteers.fold ctx.vs ~init:Volunteer.Map.empty ~f:(fun acc v ->
      Volunteer.Map.add v (daily_time_spent ctx ~by:v) acc)

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

let daily_adjusted_load (ctx : Context.t) ~of_ =
  Date.Map.mapi
    (fun date quests ->
      let open Float in
      let quests_time = total_quests_time quests in
      let adjusted_load =
        theoretical_coef ctx ~of_ ~on:date * of_int quests_time
      in
      to_int adjusted_load)
    ctx.by_day

let daily_volunteers_adjusted_load (ctx : Context.t) =
  Volunteers.fold ctx.vs ~init:Volunteer.Map.empty ~f:(fun acc v ->
      Volunteer.Map.add v (daily_adjusted_load ctx ~of_:v) acc)

(** Lower and upper bounds *)

let daily_volunteers_diff (ctx : Context.t) =
  (* TODO we could be more direct here instead of pre-calculating everything *)
  let daily_volunteers_time_spent = daily_volunteers_time_spent ctx in
  let daily_volunteers_adjusted_load = daily_volunteers_adjusted_load ctx in
  Volunteer.Map.mapi
    (fun v ->
      Date.Map.mapi (fun d time_spent ->
          let adjusted_load =
            Volunteer.Map.find v daily_volunteers_adjusted_load
            |> Date.Map.find d
          in
          Sat.(time_spent - of_int adjusted_load)))
    daily_volunteers_time_spent

let max_daily_load (ctx : Context.t) =
  60
  * Volunteers.fold ctx.vs ~init:0 ~f:(fun max_so_far v ->
      max max_so_far (Duration.to_seconds v.initial.daily_workload))

let bounds (ctx : Context.t) daily_volunteers_diff ~on =
  let s_date = Date.to_string on in
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
    let diff = Volunteer.Map.find v daily_volunteers_diff |> Date.Map.find on in
    Sat.(add ctx.model (diff <= var upper_bound));
    Sat.(add ctx.model (diff >= var lower_bound))
  in
  Sat.(var upper_bound - var lower_bound)

let sum_of_all_daily_bounds (ctx : Context.t) =
  let daily_volunteers_diff = daily_volunteers_diff ctx in
  Date.Map.fold
    (fun d _ acc -> bounds ctx daily_volunteers_diff ~on:d :: acc)
    ctx.by_day []
  |> Sat.LinearExpr.sum
