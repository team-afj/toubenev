open Lunar
open Normal

(** Normalization simplifies the representaiton for the solver, notably by
    unfolding time specifications. Concrete occurrences of quests are genrated
    for the duration of the event. *)

let expand_time_spec
    {
      Rich.Event_infos.kind = Finite { start_date; end_date };
      timezone = tz;
      _;
    } (spec : Rich.Time_spec.t) =
  let first_day =
    match spec.first_day with
    | None -> start_date
    | Some date -> Date.max start_date date
  in
  let last_day =
    match spec.last_day with
    | None -> end_date
    | Some date -> Date.min end_date date
  in
  let start_time = spec.start in
  let duration = spec.duration in
  let all_dates =
    if Date.(last_day < first_day) then []
    else
      let range = Date.Range.make ~first:first_day ~last:last_day in
      let all_days_in_range () =
        Date.Range.(
          to_list ~include_boundaries:true ~iterator:iterator_day range)
      in
      match spec.recurrence with
      | On dates -> List.filter ~f:(Fun.flip Date.Range.contains range) dates
      | Daily -> all_days_in_range ()
      | Weekly weekdays ->
          let range = all_days_in_range () in
          List.filter range ~f:(fun d ->
              let wd = Date.day_of_week d in
              Weekday.Set.mem wd weekdays)
  in
  List.map all_dates ~f:(fun date ->
      let start = Zoned_datetime.(from ~tz date start_time) in
      { Time_slot.start; duration })

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
let normalize_quest event_infos options vs diags (q : Rich.Quest.t) =
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
  let quests =
    List.mapi slots ~f:(fun i slots ->
        List.mapi slots ~f:(fun j (slot : Time_slot.t) ->
            let id = Printf.sprintf "%s_%i_%i" (Rich.id_to_string q.id) i j in
            let name =
              Printf.sprintf "%s_%s" q.name
                (Zoned_datetime.to_string slot.start)
            in
            { Quest.id; initial = q; name; slot; assigned_volunteers }))
    |> List.concat
  in
  (diagnostics, quests)

let normalize (data : Rich.Planning.t) =
  let volunteers =
    CCRAL.to_list data.volunteers
    |> List.map ~f:(normalize_volunteer data.infos)
    |> Volunteers.of_list
  in
  let quests, diagnostics =
    let diags, quests =
      CCRAL.to_list data.quests
      |> List.fold_flat_map ~init:[]
           ~f:(normalize_quest data.infos data.options volunteers)
    in
    (Quests.of_list quests, diags)
  in
  { Api.volunteers; quests; diagnostics }
