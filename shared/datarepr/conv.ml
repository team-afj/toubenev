open Lunar_jsont
open Normal

(** Normalization simplifies the representaiton for the solver, notably by
    unfolding time specifications. Concrete occurrences of quests are genrated
    for the duration of the event. *)

let expand_time_spec
    {
      Rich.Event_infos.kind = Finite { start_date; end_date };
      timezone = tz;
      day_start_local;
      _;
    } (spec : Rich.Time_spec.t) =
  let first_day, first_day_offset =
    match spec.first_day with
    | None -> (start_date, day_start_local)
    | Some date -> (Date.max start_date date, Time.midnight)
  in
  let last_day, last_day_offset =
    match spec.last_day with
    | None -> (end_date, day_start_local)
    | Some date -> (Date.min end_date date, Time.midnight)
  in
  let start_time = spec.start in
  let first =
    (* FIXME: this work only for quests with no boundary ?? *)
    Zoned_datetime.(from_date ~tz first_day + Time.to_duration first_day_offset)
  in
  let last =
    Zoned_datetime.(
      (* TODO FIXME Is this wrong ? I suspect this would lead to one additional
         occurrence after the last_day. This should maybe de done only if the
         last_day corresponds to the last event day ? *)
      from_date ~tz (Date.add_days 1 last_day)
      + Time.to_duration last_day_offset)
  in
  (* Logs.debug (fun m ->
      m "First: %s Last: %s (%s)"
        (Zoned_datetime.to_string first)
        (Zoned_datetime.to_string last)
        (Zoned_datetime.from_date ~tz last_day |> Zoned_datetime.to_string)); *)
  let range = Zoned_datetime.Range.make ~first ~last in
  let all_dates =
    if Date.(last_day < first_day) then []
    else
      let all_days_in_range () =
        Zoned_datetime.Range.(
          to_list ~include_boundaries:true ~iterator:iterator_day range)
        |> List.map ~f:Zoned_datetime.local_date
        |> Date.Set.of_list |> Date.Set.to_list
      in
      (* Logs.debug (fun m ->
          m "Dates in range: %s"
            (String.concat ~sep:", "
               (List.map (all_days_in_range ()) ~f:Date.to_string))); *)
      match spec.recurrence with
      | On dates -> dates
      | Daily -> all_days_in_range ()
      | Weekly weekdays ->
          let range = all_days_in_range () in
          List.filter range ~f:(fun d ->
              let wd = Date.day_of_week d in
              Weekday.Set.mem wd weekdays)
  in
  List.filter_map all_dates ~f:(fun date ->
      let start = Zoned_datetime.(from ~tz date start_time) in
      if Zoned_datetime.Range.contains start range then
        Some { Time_slot.start; duration = spec.duration }
      else None)

let split_time_slot (options : Rich.Options.t) { Time_slot.start; duration } =
  (* Looking for [n] such that [min <= duration / n <= max] *)
  let min_d = options.min_quest_duration in
  let max_d = options.max_quest_duration in
  let splits_duration = max_d in
  let rec aux acc start remaining =
    if Duration.(remaining <= max_d) then
      List.rev (Time_slot.{ start; duration = remaining } :: acc)
    else if Duration.(remaining <= splits_duration + min_d) then
      let acc = Time_slot.{ start; duration = min_d } :: acc in
      aux acc Zoned_datetime.(start + min_d) Duration.(remaining - min_d)
    else
      let acc = Time_slot.{ start; duration = splits_duration } :: acc in
      aux acc
        Zoned_datetime.(start + splits_duration)
        Duration.(remaining - splits_duration)
  in
  aux [] start duration

let normalize_volunteer event_infos (v : Rich.Volunteer.t) =
  let name =
    match v.public_name with Some public_name -> public_name | None -> v.name
  in
  let forbidden_tasks =
    Rich.Task_type.Set.of_list (CCRAL.to_list v.forbidden_tasks)
  in
  let wanted_tasks =
    Rich.Task_type.Set.of_list (CCRAL.to_list v.wanted_tasks)
  in
  let unwanted_tasks =
    Rich.Task_type.Set.of_list (CCRAL.to_list v.unwanted_tasks)
  in
  let unavailabilities, preferences =
    CCRAL.fold v.availabilities ~x:([], [])
      ~f:(fun (u_acc, p_acc) { Rich.Availability.status; slot } ->
        let slots = expand_time_spec event_infos slot in
        match status with
        | Unavailable -> (List.rev_append slots u_acc, p_acc)
        | Available pref ->
            (u_acc, List.rev_append (List.map slots ~f:(Pair.make pref)) p_acc))
  in
  {
    Volunteer.id = Rich.id_to_string v.id;
    name;
    initial = v;
    forbidden_tasks;
    wanted_tasks;
    unwanted_tasks;
    unavailabilities;
    preferences;
  }

(** Generates sub-quests depending of the recurrence and the task type's
    divisibility. *)
let normalize_quest event_infos options vs (diags, groups) (q : Rich.Quest.t) =
  let assigned_volunteers =
    CCRAL.fold q.assigned_volunteers ~x:Volunteer.Set.empty
      ~f:(fun acc (v : Rich.Volunteer.t) ->
        Volunteer.Set.add
          (Volunteer.Set.find_by_id (Rich.id_to_string v.id) vs)
          acc)
  in
  let slots = expand_time_spec event_infos q.slot in
  let diagnostics =
    if not (List.is_empty slots) then diags
    else
      let msg =
        let (Finite { start_date; end_date }) = event_infos.kind in
        Printf.sprintf
          "La quête \"%s\" n'a pas lieu pendant l'évènement (du %s au %s)."
          q.name
          (Date.to_string start_date)
          (Date.to_string end_date)
      in
      (Api.Warning, msg) :: diags
  in
  let slots =
    let divisible =
      match q.task_type with None -> false | Some tt -> tt.divisible
    in
    if not divisible then List.map ~f:List.pure slots
    else List.map ~f:(split_time_slot options) slots
  in
  let groups, quests =
    List.fold_flat_map_i ~init:groups slots ~f:(fun groups i slots ->
        (* [i] is the id of the expansion *)
        let group_infos =
          q.group
          |> Option.map (function
            | {
                Rich.Quests_group.id;
                name;
                recurring_quests_behavior = Same_group_for_all_occurrences;
                quests_constraint;
              } ->
                (Rich.id_to_string id, name, quests_constraint)
            | {
                id;
                name;
                recurring_quests_behavior = One_group_per_occurrence;
                quests_constraint;
              } ->
                ( Rich.(id_to_string id) ^ "_" ^ string_of_int i,
                  name,
                  quests_constraint ))
        in
        List.fold_map_i slots ~init:groups
          ~f:(fun groups j (slot : Time_slot.t) ->
            (* [j] is the id of the division *)
            let id = Printf.sprintf "%s_%i_%i" (Rich.id_to_string q.id) i j in
            let name =
              Printf.sprintf "%s_%s" q.name
                (Zoned_datetime.to_string slot.start)
            in
            let q' =
              { Quest.id; initial = q; name; slot; assigned_volunteers }
            in
            let groups =
              group_infos
              |> Option.map_or ~default:groups
                   (fun (id, name, quests_constraint) ->
                     groups
                     |> String.Map.update id @@ function
                        | None ->
                            Some
                              {
                                Normal.Quests_group.name;
                                quests = Quests.singleton q';
                                quests_constraint;
                              }
                        | Some ({ Normal.Quests_group.quests; _ } as group) ->
                            Some { group with quests = Quests.add q' quests })
            in
            (groups, q')))
  in
  ((diagnostics, groups), quests)

let normalize (data : Rich.Planning.t) =
  let volunteers =
    CCRAL.to_list data.volunteers
    |> List.map ~f:(normalize_volunteer data.infos)
    |> Volunteers.of_list
  in
  let quests, quests_groups, diagnostics =
    let (diags, quests_groups), quests =
      CCRAL.to_list data.quests
      |> List.fold_flat_map ~init:([], String.Map.empty)
           ~f:(normalize_quest data.infos data.options volunteers)
    in
    (Quests.of_list quests, quests_groups, diags)
  in
  { Api.volunteers; quests; quests_groups; diagnostics }
