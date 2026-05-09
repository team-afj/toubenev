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

let day_stats (planning : Planning.t) (normalized : data) day quests =
  let total_quest_time =
    Quests.fold quests ~init:Duration.zero ~f:(fun acc q ->
        let is_free =
          match q.initial.task_type with None -> false | Some tt -> tt.free
        in
        if is_free then acc
        else Duration.(acc + (q.slot.duration * q.initial.required_volunteers)))
  in
  let max_concurrent_volunteers =
    Quests.fold quests ~init:0 ~f:(fun acc q ->
        let overlapping = Quest.overlaps_with planning.options q quests in
        max acc
        @@ Quests.fold overlapping ~init:0 ~f:(fun acc q ->
            acc + q.initial.required_volunteers))
  in
  let volunteers =
    let day' = day in
    Volunteers.filter
      (fun v ->
        match (v.initial.arrival, v.initial.departure) with
        | None, None -> true
        | Some arrival, None -> Date.(Datetime.date arrival <= day')
        | None, Some departure -> Date.(day' <= Datetime.date departure)
        | Some arrival, Some departure ->
            Date.(Datetime.date arrival <= day')
            && Date.(day' <= Datetime.date departure))
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

let daily (planning : Planning.t) (normalized : data) =
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
