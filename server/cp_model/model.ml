open! Lunar_jsont
open Types
open Ortools
open Norm

(* Utilities *)
let is_false v = Sat.(var v == of_int 0)
let is_true v = Sat.(var v == of_int 1)

(* Constraints *)

let assume (ctx : Context.t) name =
  if ctx.with_assumptions then (
    let assumption = Sat.Var.new_bool ctx.model name in
    Sat.add_assumptions ctx.model [ assumption ];
    Some [ assumption ])
  else None

(** All quests are fully staffed *)
let all_staffed (ctx : Context.t) =
  let quest_is_staffed (q : Quest.t) =
    let open Sat in
    let name = Format.sprintf "q_%s_is_staffed" q.name in
    let sum =
      LinearExpr.sum_vars
      @@ Volunteers.to_list_map ctx.vs ~f:(fun v -> ctx.assignations v q)
    in
    let only_enforce_if =
      let name =
        Format.sprintf "%i people do %s"
          q.initial.Types.Quest.required_volunteers q.name
      in
      assume ctx name
    in
    sum
    == of_int q.initial.required_volunteers
    |> add ctx.model ~name ?only_enforce_if
  in
  ctx.for_all_quests quest_is_staffed

(** Volunteers cannot do several things at the same time *)
let non_ubiquity_of_normal_humans (ctx : Context.t) =
  ctx.for_all_quests @@ fun q ->
  let overlapping = Quest.overlaps_with ctx.options q ctx.qs in
  Quests.iter overlapping ~f:(fun q' ->
      if not (Quest.equal q q') then
        ctx.for_all_volunteers @@ fun v ->
        let assig_v = ctx.assignations v in
        let name =
          Format.sprintf "%s_cannot_do_both_%s_and_%s" v.initial.name q.name
            q'.name
        in
        Sat.Constraint.at_most_one [ assig_v q; assig_v q' ]
        |> Sat.add ctx.model ~name)

(** Not everyone is available all the time *)
let check_unavailabilities (ctx : Context.t) =
  ctx.for_all_volunteers @@ fun v ->
  List.iter v.unavailabilities ~f:(fun (slot : Time_slot.t) ->
      ctx.for_all_quests @@ fun q ->
      if Time_slot.overlaps slot q.slot then
        let name = Format.sprintf "%s_unavailable_for_%s" v.name q.name in
        let only_enforce_if =
          let name = Format.sprintf "%s not available for %s" v.name q.name in
          assume ctx name
        in
        Sat.(
          add ctx.model ~name ?only_enforce_if (is_false (ctx.assignations v q))))

(** Enforces manual assignations of volunteers, and prevents manually assigned
    volunteers from doing anything else. *)
let enforce_assignations (ctx : Context.t) =
  let assigned = Hashtbl.create 16 in
  let () =
    ctx.for_all_quests @@ fun q ->
    Volunteers.iter q.assigned_volunteers ~f:(fun (v : Volunteer.t) ->
        let name = Format.sprintf "%s_assigned_to_%s" v.name q.name in
        Hashtbl.add assigned (q.id, v.id) ();
        Sat.(add ctx.model ~name (is_true (ctx.assignations v q))))
  in
  ctx.for_all_volunteers @@ fun v ->
  if v.initial.manually_assigned then
    ctx.for_all_quests @@ fun q ->
    if not (Hashtbl.mem assigned (q.id, v.id)) then
      let name =
        Format.sprintf "manually_assigned_%s_cannot_do_%s" v.initial.name q.name
      in
      Sat.(add ctx.model ~name (is_false (ctx.assignations v q)))

(** Force every volunteer to do at least one of a quest list. Warning, this
    constraint can easily make the problem UNFEASIBLE, especially in "equal
    proportion" mode.

    Exceptions: manually assigned volunteers or with forbidden places *)
let everyone_does (ctx : Context.t) ?name requirement (quests : Quests.t) =
  let available_volunteers =
    Volunteers.filter (fun v -> not v.initial.manually_assigned) ctx.vs
  in
  Volunteers.iter available_volunteers ~f:(fun v ->
      let quests =
        Quests.filter
          (fun q ->
            not (Task_type.Set.mem q.initial.task_type v.forbidden_tasks))
          quests
      in
      match requirement with
      | `Once ->
          let name =
            Option.map (Format.sprintf "%s_do_one_%s" v.initial.name) name
          in
          let vars = Quests.to_list_map ~f:(ctx.assignations v) quests in
          Sat.add ctx.model ?name @@ Sat.Constraint.at_least_one vars
      | `Equal_proportion ->
          let n_volunteers = Volunteers.cardinal available_volunteers in
          let longuest_quest, total_time =
            Quests.fold ~init:(Duration.zero, Duration.zero) quests
              ~f:(fun (max, acc) q ->
                let max = Duration.max max q.slot.duration in
                (max, Duration.(q.slot.duration + acc)))
          in
          let longuest_quest = Duration.to_minutes longuest_quest in
          let total_time = Duration.to_minutes total_time in
          let time_per_v = (total_time / n_volunteers) - longuest_quest in
          let vars =
            Quests.to_list_map
              ~f:(fun q ->
                (Duration.to_minutes q.slot.duration, ctx.assignations v q))
              quests
          in
          let sum = Sat.LinearExpr.weighted_sum vars in
          let name =
            Option.map
              (Format.sprintf "%s_do_as_much_%s_as_everyone" v.initial.name)
              name
          in
          Sat.(add ctx.model ?name (sum >= of_int time_per_v)))

(** Force every volunteer to participate at least once to a mandatory task.
    Warning, this constraint can easily make the problem UNFEASIBLE, especially
    in "equal proportion" mode.

    Exceptions: manually assigned volunteers or with forbidden places are not
    taken into account. *)
let enforce_mandatory_tasks (ctx : Context.t) =
  let mandatory t =
    not (Equal.poly t.Task_type.everyone_should_do_it Not_necessarily)
  in
  Task_type.Set.filter mandatory ctx.task_types
  |> Task_type.Set.iter ~f:(fun tt ->
      let requirement =
        match tt.everyone_should_do_it with
        | At_least_once -> `Once
        | In_equal_proportion -> `Equal_proportion
        | Not_necessarily -> assert false
      in
      let quests =
        Quests.filter (fun q -> Task_type.equal tt q.initial.task_type) ctx.qs
      in
      everyone_does ctx ~name:tt.name requirement quests)

(** Quests groups *)

(* TODO:
   - at least one volunteer in all of them
   - or most volunteers stay the same
   - prevent volunteers to participate in the same recurring group more than
     once *)
let make ~with_assumptions (data : Planning.t) =
  let model = Sat.make ~name:"Toubenev" () in
  let context = Context.prepare ~with_assumptions model data in

  let () = all_staffed context in
  let () = non_ubiquity_of_normal_humans context in
  let () = enforce_assignations context in
  let () = enforce_mandatory_tasks context in
  let () = check_unavailabilities context in

  context

let resolve_solution (ctx : Context.t) arr =
  Array.mapi arr ~f:(fun i b ->
      try
        let name, v, q = ctx.assignations_rev i in
        Some (name, v, q, b)
      with Not_found -> None)
  |> Array.filter_map ~f:Fun.id

let pp_solution (fmt : Format.formatter) arr =
  let open Format in
  let pp_var fmt (var, _v, _q, b) =
    fprintf fmt "%s: %b" (Sat.Var.to_string var) (Bool.of_int b)
  in
  let pp_sep fmt () = fprintf fmt ";@ " in
  fprintf fmt "%a" (pp_print_array ~pp_sep pp_var) arr
