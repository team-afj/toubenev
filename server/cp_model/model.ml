open Ortools
open! Lunar_jsont
open Data_repr
open Rich
open Normal
open Shared.Static_checks

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
    let only_enforce_if =
      (* TODO: these probably add more noise than useful information *)
      (* let name =
        Format.sprintf "%i people do %s"
          q.initial.Rich.Quest.required_volunteers q.name
      in
      assume ctx name *)
      None
    in
    let vars =
      Volunteers.to_list_map ctx.vs ~f:(fun v -> ctx.assignations v q)
    in
    let constraint_ =
      (* ExactlyOne might be a bit faster *)
      if q.initial.required_volunteers = 1 then Constraint.exactly_one vars
      else LinearExpr.sum_vars vars == of_int q.initial.required_volunteers
    in
    add ctx.model ~name ?only_enforce_if constraint_
  in
  ctx.for_all_quests quest_is_staffed

(** Volunteers cannot do several things at the same time *)
let non_ubiquity_of_normal_humans (ctx : Context.t) =
  ctx.for_all_volunteers @@ fun v ->
  let manual_quests = Quests.filter (v_is_manually_assigned_to_q v) ctx.qs in
  let flexible_quests =
    Quests.filter (fun q -> not (v_is_manually_assigned_to_q v q)) ctx.qs
  in
  let intervals =
    Quests.to_list_map flexible_quests ~f:(ctx.intervals_reals v)
  in
  Sat.add_no_overlap ctx.model intervals;
  Quests.iter manual_quests ~f:(fun manual_q ->
      let overlapping =
        Quest.overlaps_with ctx.data.infos manual_q flexible_quests
      in
      Quests.iter overlapping ~f:(fun q ->
          let name =
            Format.sprintf "%s_cannot_do_%s_because_of_manual_%s" v.initial.name
              q.name manual_q.name
          in
          Sat.add ctx.model ~name (is_false (ctx.assignations v q))))

(** Not everyone is available all the time *)
let check_unavailabilities (ctx : Context.t) =
  let one_hour = Duration.from_hours 1 in
  ctx.for_all_volunteers @@ fun v ->
  ctx.for_all_quests @@ fun q ->
  (* We count prep and rest time here, but maybe it would also be reasonnable
     to ignore rest time... *)
  let q_slot = Quest.real_slot q in
  (* We add a one hour delay for people arrival / departure *)
  Option.iter
    (fun arrival ->
      if Zoned_datetime.(q_slot.start <= arrival + one_hour) then
        let name = Format.sprintf "%s_not_here_for_%s" v.name q.name in
        let only_enforce_if =
          let name = Format.sprintf "%s not here for %s" v.name q.name in
          assume ctx name
        in
        Sat.(
          add ctx.model ~name ?only_enforce_if (is_false (ctx.assignations v q))))
    v.initial.arrival;
  Option.iter
    (fun departure ->
      if Zoned_datetime.(Time_slot.end_ q_slot >= departure - one_hour) then
        let name = Format.sprintf "%s_not_here_for_%s" v.name q.name in
        let only_enforce_if =
          let name = Format.sprintf "%s not here for %s" v.name q.name in
          assume ctx name
        in
        Sat.(
          add ctx.model ~name ?only_enforce_if (is_false (ctx.assignations v q))))
    v.initial.departure;
  List.iter v.unavailabilities ~f:(fun (slot : Time_slot.t) ->
      if Time_slot.overlaps slot q_slot && not (v_is_manually_assigned_to_q v q)
      then
        let name = Format.sprintf "%s_unavailable_for_%s" v.name q.name in
        let only_enforce_if =
          let name = Format.sprintf "%s not available for %s" v.name q.name in
          assume ctx name
        in
        Sat.(
          add ctx.model ~name ?only_enforce_if (is_false (ctx.assignations v q))))

(** Some quests require specialists *)
let required_specialists (ctx : Context.t) =
  ctx.for_all_quests @@ fun q ->
  q.initial.task_type
  |> Option.iter @@ fun task_type ->
     if task_type.Task_type.specialist_only then
       ctx.for_all_volunteers @@ fun v ->
       let skills = CCRAL.to_list v.initial.proficiencies in
       if
         (not (List.mem ~eq:Task_type.equal task_type skills))
         && not (v_is_manually_assigned_to_q v q)
       then
         let name =
           Format.sprintf "%s_does_not_have_the_skill_for_%s" v.name q.name
         in
         let only_enforce_if =
           let name =
             Format.sprintf "%s does not have the skill to do %s" v.name q.name
           in
           assume ctx name
         in
         Sat.(
           add ctx.model ~name ?only_enforce_if
             (is_false (ctx.assignations v q)))

(** Some volunteers are banned from some quests types *)
let enforce_bans (ctx : Context.t) =
  ctx.for_all_volunteers @@ fun v ->
  ctx.for_all_quests @@ fun q ->
  q.initial.task_type
  |> Option.iter @@ fun task_type ->
     if Task_type.Set.mem task_type v.forbidden_tasks then
       let name =
         Format.sprintf "%s_does_not_have_the_right_to_do_%s" v.name q.name
       in
       let only_enforce_if =
         let name =
           Format.sprintf "%s does not have the right to do %s" v.name q.name
         in
         assume ctx name
       in
       Sat.(
         add ctx.model ~name ?only_enforce_if (is_false (ctx.assignations v q)))

(** Enforces manual assignations of volunteers, and prevents manually assigned
    volunteers from doing anything else. *)
let enforce_assignations (ctx : Context.t) =
  let assigned = Hashtbl.create 16 in
  let () =
    ctx.for_all_quests @@ fun q ->
    Volunteers.iter q.assigned_volunteers ~f:(fun (v : Volunteer.t) ->
        let name = Format.sprintf "%s_assigned_to_%s" v.name q.name in
        let only_enforce_if =
          assume ctx
          @@ Format.sprintf "%s is manually assigned to %s" v.initial.name
               q.name
        in
        Hashtbl.add assigned (q.id, v.id) ();
        Sat.(
          add ctx.model ~name ?only_enforce_if (is_true (ctx.assignations v q))))
  in
  ctx.for_all_volunteers @@ fun v ->
  if v.initial.manually_assigned then
    ctx.for_all_quests @@ fun q ->
    if not (Hashtbl.mem assigned (q.id, v.id)) then
      let name =
        Format.sprintf "manually_assigned_%s_cannot_do_%s" v.initial.name q.name
      in
      let only_enforce_if =
        assume ctx
        @@ Format.sprintf "%s is manually assigned to %s" v.initial.name q.name
      in
      Sat.(
        add ctx.model ~name ?only_enforce_if (is_false (ctx.assignations v q)))

(** Force every volunteer to do at least one of a quest list. Warning, this
    constraint can easily make the problem UNFEASIBLE, especially in "equal
    proportion" mode.

    Exceptions: manually assigned volunteers or with forbidden places *)
let everyone_does (ctx : Context.t) ~name requirement (quests : Quests.t) =
  let available_volunteers =
    Volunteers.filter (fun v -> not v.initial.manually_assigned) ctx.vs
  in
  let only_enforce_if =
    assume ctx @@ Format.sprintf "Tout le monde fait %s au moins une fois." name
  in
  match requirement with
  | `Once ->
      Volunteers.iter available_volunteers ~f:(fun v ->
          let quests =
            Quests.filter (fun q -> not (Quest.is_forbidden_to v q)) quests
          in
          let name = Format.sprintf "%s_do_one_%s" v.initial.name name in
          let vars = Quests.to_list_map ~f:(ctx.assignations v) quests in
          Sat.add ctx.model ~name ?only_enforce_if
          @@ Sat.Constraint.at_least_one vars)
  | `Only_once ->
      Volunteers.iter available_volunteers ~f:(fun v ->
          let quests =
            Quests.filter (fun q -> not (Quest.is_forbidden_to v q)) quests
          in
          let name = Format.sprintf "%s_do_%s_only_once" v.initial.name name in
          let vars = Quests.to_list_map ~f:(ctx.assignations v) quests in
          Sat.add ctx.model ~name ?only_enforce_if
          @@ Sat.Constraint.at_most_one vars)
  | `Equal_proportion ->
      let n_volunteers = Volunteers.cardinal available_volunteers in
      let n_slots =
        Quests.fold ~init:0 quests ~f:(fun acc q ->
            acc + Quest.required_volunteers ~filter:`Manually_assigned q)
      in
      Volunteers.iter available_volunteers ~f:(fun v ->
          let quests =
            Quests.filter (fun q -> not (Quest.is_forbidden_to v q)) quests
          in
          let n_slots_assigned_to_v =
            Quests.fold ~init:0 quests ~f:(fun acc_ass q ->
                if v_is_manually_assigned_to_q v q then acc_ass + 1 else acc_ass)
          in
          let max_slots_per_v = 1 + (n_slots / n_volunteers) in
          let v_max_slots = max max_slots_per_v n_slots_assigned_to_v in
          let vars = Quests.to_list_map ~f:(ctx.assignations v) quests in
          let sum = Sat.LinearExpr.sum_vars vars in
          let name =
            Format.sprintf "%s_do_as_much_%s_as_everyone" v.initial.name name
          in
          Sat.(add ctx.model ~name ?only_enforce_if (sum <= of_int v_max_slots)))

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
        | At_most_once -> `Only_once
        | In_equal_proportion -> `Equal_proportion
        | Not_necessarily -> assert false
      in
      let quests =
        Quests.filter
          (fun q ->
            match q.initial.task_type with
            | None -> true
            | Some tt' -> Task_type.equal tt tt')
          ctx.qs
      in
      everyone_does ctx ~name:tt.name requirement quests)

(** Some people refuse to work with other people *)
let know_your_ennemy (ctx : Context.t) =
  let processed_pairs : (string * string, unit) Hashtbl.t = Hashtbl.create 32 in
  ctx.for_all_volunteers @@ fun (v : Volunteer.t) ->
  List.iter v.initial.ennemis ~f:(fun ennemy_uuid ->
      let ennemy_id = id_to_string ennemy_uuid in
      if not (String.equal v.id ennemy_id) then
        let a, b =
          if String.compare v.id ennemy_id <= 0 then (v.id, ennemy_id)
          else (ennemy_id, v.id)
        in
        if not (Hashtbl.mem processed_pairs (a, b)) then
          match Volunteers.find_by_id ennemy_id ctx.vs with
          | exception Not_found -> ()
          | ennemy ->
              Hashtbl.add processed_pairs (a, b) ();
              ctx.for_all_quests @@ fun q ->
              let name =
                Format.sprintf "%s_cannot_do_%s_with_%s" v.name q.name
                  ennemy.name
              in
              let only_enforce_if =
                assume ctx
                @@ Format.sprintf "%s cannot work with %s on %s" v.name
                     ennemy.name q.name
              in
              Sat.Constraint.at_most_one
                [ ctx.assignations v q; ctx.assignations ennemy q ]
              |> Sat.add ctx.model ~name ?only_enforce_if)

(** Dstribute breaks *)

let distribute_break (ctx : Context.t) { Break.initial; slot } =
  (* TODO volunteer filters *)
  (* TODO we could magnetize break slots to start every fifteen minutes.
     It would scale the domain down and make more pleasants results. *)
  let duration = initial.duration in
  let duration_m = Duration.to_minutes duration in
  let start_m = Zoned_datetime.to_local_minutes slot.start in
  let start_s = Zoned_datetime.to_string slot.start in
  let end_m = Zoned_datetime.to_local_minutes (Time_slot.end_ slot) in
  let overlapping_quests =
    Quests.filter (fun q -> Time_slot.overlaps slot q.slot) ctx.qs
    |> Quests.to_list
  in
  ctx.for_all_volunteers @@ fun v ->
  let start =
    let name =
      String.concat ~sep:"_"
        [ "break_start"; initial.name; "for"; v.name; "on"; start_s ]
    in
    let lb = start_m in
    let ub =
      end_m - duration_m
      (* The break must start [duration_of_the_break] before the break slot ends *)
    in
    Sat.Var.new_int ctx.model ~lb ~ub name
  in
  let break =
    let name =
      String.concat ~sep:"_"
        [ "break_interval"; initial.name; "for"; v.name; "on"; start_s ]
    in
    let start = Sat.var start in
    let size = Sat.of_int duration_m in
    let end_ = Sat.(start + size) in
    Sat.new_interval_var ctx.model ~start ~size ~end_ name
  in
  let name =
    String.concat ~sep:"_"
      [ "break_no_overlap"; initial.name; "for"; v.name; "on"; start_s ]
  in
  let only_enforce_if =
    assume ctx
    @@ String.concat ~sep:" "
         [
           "Break ";
           initial.name;
           "for";
           v.name;
           "on";
           start_s;
           "does not overlap with other quests";
         ]
  in
  let intervals = break :: List.map overlapping_quests ~f:(ctx.intervals v) in
  Sat.add_no_overlap ctx.model ?only_enforce_if ~name intervals

let distribute_breaks (ctx : Context.t) =
  List.iter ~f:(distribute_break ctx) ctx.breaks

(** Quests groups TODO remaining constraints*)
let handle_grouped_quests (ctx : Context.t) =
  let enforce_group id
      { Quests_group.name = group_name; quests; quests_constraint } =
    match quests_constraint with
    | Maximum_common_volunteers ->
        let quests = Quests.to_list quests in
        let sorted_quests =
          List.sort
            ~cmp:(fun (q : Quest.t) (q' : Quest.t) ->
              Int.compare q.initial.required_volunteers
                q'.initial.required_volunteers)
            quests
        in
        ctx.for_all_volunteers @@ fun v ->
        let assignation q = ctx.assignations v q in
        let name =
          Format.sprintf "if_%s_do_%s_they_do_all_quests_in_group_%s[%s]" v.name
            (List.hd sorted_quests).name group_name id
        in
        let assume =
          assume ctx
          @@ Format.sprintf "if %s do %s they do all quests in group %s[%s]"
               v.name (List.hd sorted_quests).name group_name id
        in
        let assignations = List.map ~f:assignation sorted_quests in
        let doing_least_volunteer_required, others = List.hd_tl assignations in
        let all_others = Sat.Constraint.And others in
        (* doing least_volunteer_required implies doing the others quests *)
        let only_enforce_if =
          match assume with
          | None -> [ doing_least_volunteer_required ]
          | Some assumption -> doing_least_volunteer_required :: assumption
        in
        Sat.add ctx.model ~name ~only_enforce_if all_others
    | Distinct_volunteers ->
        (* [v] can do only one of [quests] *)
        ctx.for_all_volunteers @@ fun v ->
        let vars = Quests.to_list_map ~f:(ctx.assignations v) quests in
        let name =
          Format.sprintf "%s_can_only_do_one_of_the_quests_in_group_%s[%s]"
            v.name group_name id
        in
        let only_enforce_if =
          assume ctx
          @@ Format.sprintf "%s can only do one of the quests in group %s[%s]"
               v.name group_name id
        in
        let sum = Sat.LinearExpr.sum_vars vars in
        Sat.add ctx.model ~name ?only_enforce_if Sat.(sum <= of_int 1)
    | _ -> failwith "not implemented"
  in
  String.Map.iter enforce_group ctx.quests_groups

(* TODO:
   - at least one volunteer in all of them
   - or most volunteers stay the same
   - prevent volunteers to participate in the same recurring group more than
     once *)

(** Objective *)

let friendship_bonus (ctx : Context.t) =
  let processed_pairs : (string * string, unit) Hashtbl.t = Hashtbl.create 32 in
  Quests.fold ctx.qs ~init:[] ~f:(fun acc q ->
      Volunteers.fold ctx.vs ~init:acc ~f:(fun acc v ->
          List.fold_left v.initial.friends ~init:acc ~f:(fun acc friend_uuid ->
              let friend_id = id_to_string friend_uuid in
              if String.equal v.id friend_id then acc
              else
                let oredered_pair =
                  if String.compare v.id friend_id <= 0 then (v.id, friend_id)
                  else (friend_id, v.id)
                in
                if Hashtbl.mem processed_pairs oredered_pair then acc
                else begin
                  Hashtbl.add processed_pairs oredered_pair ();
                  let friend = Volunteers.find_by_id friend_id ctx.vs in
                  let together_name =
                    Format.sprintf "%s_and_%s_together_on_%s" v.name friend.name
                      q.name
                  in
                  let together = Sat.Var.new_bool ctx.model together_name in
                  Sat.add ctx.model
                    (Sat.Constraint.min_equality together
                       [
                         Sat.LinearExpr.var (ctx.assignations v q);
                         Sat.LinearExpr.var (ctx.assignations friend q);
                       ]);
                  together :: acc
                end)))
  |> Sat.LinearExpr.sum_vars

(** Appreciation score for a volunteer doing a specific quest based on time
    preferences. Breaks down the quest into 15-minute blocks and sums preference
    scores for each block where a volunteer's preferred time slot overlaps. *)
let appreciation_of_quest (opt : Options.t) (v : Volunteer.t) (q : Quest.t) =
  let quest_end = Time_slot.end_ q.slot in
  let fifteen_minutes = Duration.from_minutes 15 in
  let quest_type_score =
    q.initial.task_type
    |> Option.map_or ~default:0 @@ fun quest_type ->
       if Task_type.Set.mem quest_type v.wanted_tasks then
         opt.desired_quest_bonus
       else if Task_type.Set.mem quest_type v.unwanted_tasks then
         -1 * opt.undesired_quest_bonus
       else 0
  in
  let rec loop acc current =
    if Zoned_datetime.(current >= quest_end) then acc
    else
      let next_time =
        Zoned_datetime.(min (current + fifteen_minutes) quest_end)
      in
      let block_duration =
        Duration.(
          Zoned_datetime.to_local_duration next_time
          - Zoned_datetime.to_local_duration current)
      in
      let current_block =
        { Time_slot.start = current; duration = block_duration }
      in
      let preferences_score =
        List.fold_left v.preferences ~init:0
          ~f:(fun sum (pref_score, pref_slot) ->
            if Time_slot.overlaps current_block pref_slot then sum + pref_score
            else sum)
      in
      loop (acc + quest_type_score + preferences_score) next_time
  in
  loop 0 q.slot.start

(** Total appreciation of a planning for a volunteer. Sums the appreciation of
    each quest, weighted by whether the volunteer is assigned. *)
let appreciation_of_planning opts (ctx : Context.t) =
  Volunteers.fold ctx.vs ~init:[] ~f:(fun acc v ->
      Quests.fold ctx.qs ~init:acc ~f:(fun acc q ->
          let appreciation = appreciation_of_quest opts v q in
          Logs.debug (fun msg -> msg "%s + %s = %i" v.name q.name appreciation);
          if appreciation = 0 then acc
          else
            Sat.scale appreciation (Sat.LinearExpr.var (ctx.assignations v q))
            :: acc))
  |> Sat.LinearExpr.sum (* maybe weighted sum *)

(* Amplitudes: daily work time span *)

let quest_time_range (ctx : Context.t) (v : Volunteer.t) (quests : Quests.t) =
  (* To reduce the domain, we don't count every minutes *)
  let granulatity_m = 60. in
  let approx minutes =
    Float.(of_int minutes / granulatity_m |> round |> to_int)
  in
  (* let first_starting_quest =
    Quests.fold quests ~init:None ~f:(fun acc q ->
        match acc with
        | None -> Some q
        | Some q' ->
            if Zoned_datetime.(q.slot.start < q'.slot.start) then Some q
            else Some q')
    |> Option.get
  in *)
  let last_ending_quest =
    Quests.fold quests ~init:None ~f:(fun acc q ->
        match acc with
        | None -> Some q
        | Some q' ->
            if Zoned_datetime.(Time_slot.end_ q.slot > Time_slot.end_ q'.slot)
            then Some q
            else Some q')
    |> Option.get
  in
  let lb = 0 in
  let ub =
    approx
    @@ Zoned_datetime.to_local_minutes (Time_slot.end_ last_ending_quest.slot)
  in
  let start = Sat.Var.new_int ctx.model ~lb ~ub (v.name ^ "_day_start") in
  let end_ = Sat.Var.new_int ctx.model ~lb ~ub (v.name ^ "_day_end") in
  let v_day_quests_start =
    Quests.to_list_map quests ~f:(fun q ->
        let start_stamp =
          approx @@ Zoned_datetime.to_local_minutes q.slot.start
        in
        Sat.scale start_stamp (Sat.var (ctx.assignations v q)))
  in
  let v_day_quests_end =
    Quests.to_list_map quests ~f:(fun q ->
        let start_stamp =
          approx @@ Zoned_datetime.to_local_minutes (Time_slot.end_ q.slot)
        in
        Sat.scale start_stamp (Sat.var (ctx.assignations v q)))
  in
  let min_equality = Sat.Constraint.min_equality start v_day_quests_start in
  let max_equality = Sat.Constraint.max_equality end_ v_day_quests_end in
  let () =
    Sat.add ctx.model ~name:(v.name ^ "min_quest_time_range") min_equality;
    Sat.add ctx.model ~name:(v.name ^ "max_quest_time_range") max_equality
  in
  Sat.(var end_ - var start)

let amplitudes (ctx : Context.t) =
  Volunteers.fold ctx.vs ~init:(Sat.of_int 0) ~f:(fun acc v ->
      Date.Map.fold
        (fun _day day_quests acc ->
          Sat.(acc + quest_time_range ctx v day_quests))
        ctx.by_day acc)

let minimize_f (ctx : Context.t) =
  let options = ctx.data.options in
  let event_bounds_coef = options.event_equilibrium_malus in
  let daily_bounds_coef = options.daily_equilibrium_malus in
  let amplitude_coef =
    0
    (*options.large_amplitude_malus*)
  in
  let friendship_coef = options.friendship_bonus in
  let resolution = `Minutes in
  let open Sat.LinearExpr in
  let objective_terms =
    [
      scale (15 * 2 * 10 * 10 * event_bounds_coef)
      @@ Workload_balance.event_bounds ctx resolution;
      scale (15 * 10 * 10 * daily_bounds_coef)
      @@ Workload_balance.daily_bounds ctx resolution;
      scale (-1 * friendship_coef) @@ friendship_bonus ctx;
      scale (-1) @@ appreciation_of_planning options ctx;
    ]
  in
  let objective_terms =
    if amplitude_coef = 0 then objective_terms
    else (scale amplitude_coef @@ amplitudes ctx) :: objective_terms
  in
  sum objective_terms |> Sat.minimize ctx.model

let make ?(no_optim = false) ~with_assumptions (data : Planning.t) =
  let start_time = Unix.gettimeofday () in
  let model = Sat.make ~name:"Toubenev" () in
  let context = Context.prepare ~with_assumptions model data in

  let () = all_staffed context in
  let () = non_ubiquity_of_normal_humans context in
  let () = enforce_assignations context in
  let () = enforce_mandatory_tasks context in
  let () = check_unavailabilities context in
  let () = required_specialists context in
  let () = enforce_bans context in
  let () = know_your_ennemy context in
  let () = handle_grouped_quests context in
  let () = distribute_breaks context in

  let () = if not (with_assumptions || no_optim) then minimize_f context in

  Logs.debug (fun m ->
      m "Model building took %fs" (Unix.gettimeofday () -. start_time));
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
