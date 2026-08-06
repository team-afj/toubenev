open Ortools
open! Lunar_jsont
open Data_repr
open! Rich
open Normal
open Shared.Workload_analysis

(* Things to keep in mind:
   - Volunteer have arrival adn departure times
   - Some quests are free: they don't count in the volunteer workload
*)

(** Real assignations *)

(* THIS could be share *)

let one_hour = 60

(** [time_spent ctx ~by:volunteer ~on:quests] sums the duration of each quest in
    [quests] that is assigned to [volunteer]. *)
let time_spent (ctx : Context.t) ~unit ~by:volunteer ~on:quests =
  Sat.LinearExpr.weighted_sum
  @@ Quests.to_list_map quests ~f:(fun q ->
      let assigned = ctx.assignations volunteer q in
      let duration = Quest.real_duration ~unit q in
      (duration, assigned))

(** Diffs *)

(** Returns the difference between the theoretical load of the volunteer and the
    actual load in the current solution. *)
let load_diff (ctx : Context.t) resolution volunteer day day_quests =
  let unit = resolution in
  let time_spent = time_spent ctx ~unit ~by:volunteer ~on:day_quests in
  let adjusted_load =
    adjusted_load_minutes ctx.static_checks ~unit ctx.vs volunteer day
      day_quests
  in
  Sat.(time_spent - of_int adjusted_load)

(** Lower and upper bounds *)

let max_daily_load (ctx : Context.t) unit =
  Volunteers.fold ctx.vs ~init:0 ~f:(fun max_so_far v ->
      max max_so_far (Duration.to_minutes v.initial.daily_workload))
  |> Float.of_int |> Quest.minutes_conv ~unit |> Float.to_int

let bounds (ctx : Context.t) resolution day day_quests =
  let s_date = Date.to_string day in
  let max_daily_load = max_daily_load ctx resolution in
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
  let diffs =
    Volunteers.to_list_map ctx.vs ~f:(fun v ->
        load_diff ctx resolution v day day_quests)
  in
  Sat.(add ctx.model (Constraint.max_equality upper_bound diffs));
  Sat.(add ctx.model (Constraint.min_equality lower_bound diffs));
  Sat.(var upper_bound - var lower_bound)

(** [daily_bounds], depending on [resolution] vary:
    - in minutes between 0 and 1440 24 * 60
    - in fifteen_minutes between 0 and 96 *)
let daily_bounds (ctx : Context.t) resolution =
  Date.Map.fold
    (fun day day_quests acc -> bounds ctx resolution day day_quests :: acc)
    ctx.by_day []
  |> Sat.LinearExpr.sum

(** [event_bound], depending on [resolution] vary:
    - in minutes between 0 and 1440 24 * 60
    - in fifteen_minutes between 0 and 96 *)
let event_bounds (ctx : Context.t) resolution =
  let max_daily_load = max_daily_load ctx resolution in
  let lb = -2 * max_daily_load in
  let ub = 2 * max_daily_load in
  let lower_bound =
    Printf.sprintf "diff_lower_bound_event" |> Sat.Var.new_int ctx.model ~lb ~ub
  in
  let upper_bound =
    Printf.sprintf "diff_upper_bound_day_event"
    |> Sat.Var.new_int ctx.model ~lb ~ub
  in
  let diffs =
    Volunteers.to_list_map ctx.vs ~f:(fun v ->
        Date.Map.fold
          (fun day day_quests acc ->
            Sat.(load_diff ctx resolution v day day_quests + acc))
          ctx.by_day (Sat.of_int 0))
  in
  Sat.(add ctx.model (Constraint.max_equality upper_bound diffs));
  Sat.(add ctx.model (Constraint.min_equality lower_bound diffs));
  Sat.(var upper_bound - var lower_bound)

let v_day_abs_diffs (ctx : Context.t) resolution (v : Volunteer.t)
    (day, day_quests) =
  let name = Printf.sprintf "abs_diff_day_%s_%s" (Date.to_string day) v.name in
  let event_abs_diff =
    let lb = 0 in
    let ub = 60 * 24 in
    name |> Sat.Var.new_int ctx.model ~lb ~ub
  in
  let abs =
    Sat.Constraint.abs_equality event_abs_diff
      [ load_diff ctx resolution v day day_quests ]
  in
  Sat.add ctx.model ~name abs;
  event_abs_diff

let v_days_abs_diffs (ctx : Context.t) resolution (v : Volunteer.t) =
  Date.Map.to_list ctx.by_day
  |> List.map ~f:(v_day_abs_diffs ctx resolution v)
  |> Sat.LinearExpr.sum_vars

let days_abs_diffs (ctx : Context.t) resolution =
  let abs_diffs =
    Volunteers.to_list_map ctx.vs ~f:(v_days_abs_diffs ctx resolution)
  in
  Sat.LinearExpr.sum abs_diffs

let v_event_abs_diffs (ctx : Context.t) resolution (v : Volunteer.t) =
  let name = Printf.sprintf "abs_diff_event_%s" v.name in
  let event_abs_diff =
    let lb = 0 in
    let ub = 60 * 24 in
    name |> Sat.Var.new_int ctx.model ~lb ~ub
  in
  let abs =
    Sat.Constraint.abs_equality event_abs_diff
      [
        Date.Map.fold
          (fun day day_quests acc ->
            Sat.(load_diff ctx resolution v day day_quests + acc))
          ctx.by_day (Sat.of_int 0);
      ]
  in
  Sat.add ctx.model ~name abs;
  event_abs_diff

let event_abs_diffs (ctx : Context.t) resolution =
  let abs_diffs =
    Volunteers.to_list_map ctx.vs ~f:(v_event_abs_diffs ctx resolution)
  in
  Sat.LinearExpr.sum_vars abs_diffs

let v_event_pow_diffs (ctx : Context.t) resolution (v : Volunteer.t) =
  let name = Printf.sprintf "abs_diff_event_%s" v.name in
  let event_abs_diff =
    let lb = 0 in
    let ub = 60 * 24 in
    name |> Sat.Var.new_int ctx.model ~lb ~ub
  in
  let pow =
    let v =
      Date.Map.fold
        (fun day day_quests acc ->
          Sat.(load_diff ctx resolution v day day_quests + acc))
        ctx.by_day (Sat.of_int 0)
    in
    Sat.Constraint.multiplication_equality event_abs_diff [ v; v ]
  in
  Sat.add ctx.model ~name pow;
  event_abs_diff

let event_pow_diffs (ctx : Context.t) resolution =
  let abs_diffs =
    Volunteers.to_list_map ctx.vs ~f:(v_event_abs_diffs ctx resolution)
  in
  Sat.LinearExpr.sum_vars abs_diffs

let v_day_pow_diff (ctx : Context.t) resolution (v : Volunteer.t) day day_quests
    =
  let s_date = Date.to_string day in
  let name = Printf.sprintf "pow_diff_%s_day_%s" v.name s_date in
  let v_day_pow_diff =
    let lb = 0 in
    let ub = 60 * 24 in
    name |> Sat.Var.new_int ctx.model ~lb ~ub
  in
  let pow =
    let v = (load_diff ctx resolution v day day_quests) in
    Sat.Constraint.multiplication_equality v_day_pow_diff [ v; v ]
  in
  Sat.add ctx.model ~name pow;
  v_day_pow_diff

let daily_pow_diffs (ctx : Context.t) resolution =
  Volunteers.fold ctx.vs ~init:[] ~f:(fun acc v ->
      Date.Map.fold
        (fun day day_quests acc ->
          v_day_pow_diff ctx resolution v day day_quests :: acc)
        ctx.by_day acc)
  |> Sat.LinearExpr.sum_vars
