open Lunar
open Data_repr
open Rich
open Normal

type daily = { total_quest_time : Duration.t }

let quests_stats quests =
  let total_quest_time =
    Quests.fold quests ~init:Duration.zero ~f:(fun acc q ->
        Duration.(acc + (q.slot.duration * q.initial.required_volunteers)))
  in
  { total_quest_time }

let daily (infos : Event_infos.t) { quests; _ } =
  let by_day = quests_by_day infos quests in
  Date.Map.map quests_stats by_day
