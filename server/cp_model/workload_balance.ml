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
let load_diff (ctx : Context.t) volunteer day day_quests =
  let unit =
    `Fifteen_minutes
    (* `Five_minutes should be faster but does not give good results right now... *)
  in
  let time_spent = time_spent ctx ~unit ~by:volunteer ~on:day_quests in
  let adjusted_load =
    adjusted_load_minutes ctx.data.infos ~unit ctx.vs volunteer day day_quests
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
    Volunteers.to_list_map ctx.vs ~f:(fun v -> load_diff ctx v day day_quests)
  in
  Sat.(add ctx.model (Constraint.max_equality upper_bound diffs));
  Sat.(add ctx.model (Constraint.min_equality lower_bound diffs));
  Sat.(var upper_bound - var lower_bound)

let daily_bounds (ctx : Context.t) resolution =
  Date.Map.fold
    (fun day day_quests acc -> bounds ctx resolution day day_quests :: acc)
    ctx.by_day []
  |> Sat.LinearExpr.sum

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
          (fun day day_quests acc -> Sat.(load_diff ctx v day day_quests + acc))
          ctx.by_day (Sat.of_int 0))
  in
  Sat.(add ctx.model (Constraint.max_equality upper_bound diffs));
  Sat.(add ctx.model (Constraint.min_equality lower_bound diffs));
  Sat.(var upper_bound - var lower_bound)
