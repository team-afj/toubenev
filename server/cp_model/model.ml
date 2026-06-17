open Ortools
open! Lunar_jsont
open Data_repr
open Rich
open Normal

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
      (* TODO: these probably add more noise than useful information *)
      (* let name =
        Format.sprintf "%i people do %s"
          q.initial.Rich.Quest.required_volunteers q.name
      in
      assume ctx name *)
      None
    in
    sum
    == of_int q.initial.required_volunteers
    |> add ctx.model ~name ?only_enforce_if
  in
  ctx.for_all_quests quest_is_staffed

(** Volunteers cannot do several things at the same time *)
let non_ubiquity_of_normal_humans (ctx : Context.t) =
  ctx.for_all_quests @@ fun q ->
  let overlapping = Quest.overlaps_with ctx.data.infos q ctx.qs in
  Quests.iter overlapping ~f:(fun q' ->
      if not (Quest.equal q q') then
        ctx.for_all_volunteers @@ fun v ->
        let assig_v = ctx.assignations v in
        let name =
          Format.sprintf "%s_cannot_do_both_%s_and_%s" v.initial.name q.name
            q'.name
        in
        let only_enforce_if =
          (* TODO: these probably add more noise than useful information *)
          (* assume ctx
          @@ Format.sprintf "%s cannot do both %s and %s" v.initial.name q.name
               q'.name *)
          None
        in
        Sat.Constraint.at_most_one [ assig_v q; assig_v q' ]
        |> Sat.add ctx.model ?only_enforce_if ~name)

(** Not everyone is available all the time *)
let check_unavailabilities (ctx : Context.t) =
  let one_hour = Duration.from_hours 1 in
  ctx.for_all_volunteers @@ fun v ->
  ctx.for_all_quests @@ fun q ->
  (* We consider a one hour delay for people arrival / departure *)
  Option.iter
    (fun arrival ->
      if Zoned_datetime.(q.slot.start <= arrival + one_hour) then
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
      if Zoned_datetime.(Time_slot.end_ q.slot >= departure - one_hour) then
        let name = Format.sprintf "%s_not_here_for_%s" v.name q.name in
        let only_enforce_if =
          let name = Format.sprintf "%s not here for %s" v.name q.name in
          assume ctx name
        in
        Sat.(
          add ctx.model ~name ?only_enforce_if (is_false (ctx.assignations v q))))
    v.initial.departure;
  List.iter v.unavailabilities ~f:(fun (slot : Time_slot.t) ->
      if Time_slot.overlaps slot q.slot then
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
       if not (List.mem ~eq:Task_type.equal task_type skills) then
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
  Volunteers.iter available_volunteers ~f:(fun v ->
      let quests =
        Quests.filter
          (fun q ->
            match q.initial.task_type with
            | None -> true
            | Some tt -> not (Task_type.Set.mem tt v.forbidden_tasks))
          quests
      in
      match requirement with
      | `Once ->
          let name = Format.sprintf "%s_do_one_%s" v.initial.name name in
          let vars = Quests.to_list_map ~f:(ctx.assignations v) quests in
          Sat.add ctx.model ~name ?only_enforce_if
          @@ Sat.Constraint.at_least_one vars
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
            Format.sprintf "%s_do_as_much_%s_as_everyone" v.initial.name name
          in
          Sat.(add ctx.model ~name ?only_enforce_if (sum >= of_int time_per_v)))

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
                @@ Format.sprintf "%s wannot work with %s on %s" v.name
                     ennemy.name q.name
              in
              Sat.Constraint.at_most_one
                [ ctx.assignations v q; ctx.assignations ennemy q ]
              |> Sat.add ctx.model ~name ?only_enforce_if)

(* TODO Daily break *)

(** TODO Quests groups *)

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
          Sat.scale appreciation (Sat.LinearExpr.var (ctx.assignations v q))
          :: acc))
  |> Sat.LinearExpr.sum

let minimize_f (ctx : Context.t) =
  let options = ctx.data.options in
  let event_bounds_coef = options.event_equilibrium_malus in
  let daily_bounds_coef = options.daily_equilibrium_malus in
  let friendship_coef = options.friendship_bonus in
  let open Sat.LinearExpr in
  [
    scale (10 * event_bounds_coef) @@ Workload_balance.event_bounds ctx;
    scale (10 * daily_bounds_coef) @@ Workload_balance.daily_bounds ctx;
    scale (-1 * friendship_coef) @@ friendship_bonus ctx;
    scale (-1) @@ appreciation_of_planning options ctx;
  ]
  |> sum |> Sat.minimize ctx.model

let make ~with_assumptions (data : Planning.t) =
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

  let () = if not with_assumptions then minimize_f context in

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
