open Lunar_jsont
open Data_repr
open Rich
open Normal

type daily = {
  total_quest_time : Duration.t;
  total_volunteer_time : Duration.t;
  max_concurrent_volunteers : int;
  available_volunteers : int;
}

type t = { daily : daily Date.Map.t }

let day_stats (_planning : Planning.t) (normalized : Api.data) day quests =
  let total_quest_time =
    Quests.fold quests ~init:Duration.zero ~f:(fun acc q ->
        let is_free =
          match q.initial.task_type with None -> false | Some tt -> tt.free
        in
        if is_free then acc
        else Duration.(acc + (q.slot.duration * q.initial.required_volunteers)))
  in
  let max_concurrent_volunteers =
    (* Classical two-steps algorithm for max interval overlap. Sort the list by
       start time and with additional weights. Then sweep the list to accumulate
       the weight and remember the maximum. *)
    let events =
      Quests.fold quests ~init:Zoned_datetime.Map.empty ~f:(fun acc q ->
          let required_volunteers = q.initial.required_volunteers in
          let add_delta acc time delta =
            Zoned_datetime.Map.update time
              (function
                | None -> Some delta
                | Some existing_delta -> Some (existing_delta + delta))
              acc
          in
          let acc = add_delta acc q.slot.start required_volunteers in
          add_delta acc (Time_slot.end_ q.slot) (-required_volunteers))
    in
    Zoned_datetime.Map.fold
      (fun _time delta (current_active, current_peak) ->
        let current_active = current_active + delta in
        (current_active, max current_peak current_active))
      events (0, 0)
    |> snd
  in
  let volunteers =
    let day' = day in
    Volunteers.filter
      (fun v ->
        match (v.initial.arrival, v.initial.departure) with
        | None, None -> true
        | Some arrival, None -> Date.(Zoned_datetime.local_date arrival <= day')
        | None, Some departure ->
            Date.(day' <= Zoned_datetime.local_date departure)
        | Some arrival, Some departure ->
            Date.(Zoned_datetime.local_date arrival <= day')
            && Date.(day' <= Zoned_datetime.local_date departure))
      normalized.volunteers
  in
  let total_volunteer_time =
    Volunteers.fold volunteers ~init:Duration.zero ~f:(fun acc v ->
        Duration.(acc + v.initial.daily_workload))
  in
  let available_volunteers = Volunteers.cardinal volunteers in
  {
    total_quest_time;
    total_volunteer_time;
    max_concurrent_volunteers;
    available_volunteers;
  }

let daily (planning : Planning.t) (normalized : Api.data) =
  let by_day = quests_by_day planning.infos normalized.quests in
  Date.Map.mapi (day_stats planning normalized) by_day

let of_planning infos n = { daily = daily infos n }

let diags { daily } =
  Date.Map.fold
    (fun d { max_concurrent_volunteers; available_volunteers; _ } acc ->
      if available_volunteers >= max_concurrent_volunteers then acc
      else
        let msg =
          Printf.sprintf
            "Pas assez de bénévoles %s %i pour assurer toutes les quêtes lors \
             du coup de feu."
            (Date.weekday d |> Weekday.to_intl_short_string `Fr)
            (Date.day_of_month d)
        in
        (Api.Error, msg) :: acc)
    daily []
