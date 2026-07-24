open! Lunar_jsont
open Data_repr
open! Rich
open Normal

(** Theoretical targets *)
let total_quests_time quests =
  (* We don't count manually assigned quests as to not bias the proportions
     since manually assigned volunteers will not take part in other quests. *)
  Quests.fold quests ~init:0 ~f:(fun acc q ->
      acc
      + Quest.weighted_duration ~skip_manually_assigned:true ~unit:`Minutes q)
  |> Duration.from_minutes

let max_doable_non_overlapping_duration infos (volunteer : Volunteer.t) quests =
  let doable_quests =
    Quests.filter (Static_checks.v_can_do_q infos quests volunteer) quests
    |> Quests.to_list
    |> List.sort ~cmp:(fun q1 q2 ->
        Zoned_datetime.compare
          (Time_slot.end_ (Quest.real_slot q1))
          (Time_slot.end_ (Quest.real_slot q2)))
  in
  let quests = Array.of_list doable_quests in
  let quest_count = Array.length quests in
  if quest_count = 0 then Duration.zero
  else
    let ends =
      Array.init quest_count ~f:(fun i ->
          Time_slot.end_ (Quest.real_slot quests.(i)))
    in
    let best_until = Array.make quest_count Duration.zero in
    (* Rightmost index <= [hi] whose quest end is <= [start_time]. *)
    let predecessor_index start_time hi =
      let rec aux lo hi best_idx =
        if lo > hi then best_idx
        else
          let mid = (lo + hi) / 2 in
          if Zoned_datetime.(ends.(mid) <= start_time) then
            aux (mid + 1) hi (Some mid)
          else aux lo (mid - 1) best_idx
      in
      aux 0 hi None
    in
    for i = 0 to quest_count - 1 do
      let q = quests.(i) in
      let q_slot = Quest.real_slot q in
      let best_before_q =
        match predecessor_index q_slot.start (i - 1) with
        | Some j -> best_until.(j)
        | None -> Duration.zero
      in
      let best_with_q = Duration.(best_before_q + q.slot.duration) in
      let best_without_q =
        if i = 0 then Duration.zero else best_until.(i - 1)
      in
      best_until.(i) <- Duration.max best_without_q best_with_q
    done;
    best_until.(quest_count - 1)

let theoretical_load infos ~of_:(volunteer : Volunteer.t) ~on:date day_quests =
  (* TODO Maybe check other factors ? Pro rata of arrival time ? *)
  match (volunteer.initial.arrival, volunteer.initial.departure) with
  | Some arrival, _ when Date.(date < Zoned_datetime.local_date arrival) ->
      `Fixed Duration.zero
  | _, Some departure when Date.(Zoned_datetime.local_date departure < date) ->
      `Fixed Duration.zero
  | _ ->
      let available_hours =
        Volunteer.available_hours infos ~on:date volunteer
        (* max_doable_non_overlapping_duration infos volunteer day_quests *)
      in
      let theory =
        Duration.min volunteer.initial.daily_workload available_hours
      in
      let manually_assigned =
        Quests.fold day_quests ~init:Duration.zero ~f:(fun acc q ->
            if
              (not (Quest.is_free q))
              && Static_checks.v_is_manually_assigned_to_q volunteer q
            then Duration.(acc + q.slot.duration)
            else acc)
      in
      let final = Duration.max theory manually_assigned in
      if Duration.(equal zero available_hours) then `Fixed final
      else `Flexible final

let total_theoretical_load infos volunteers ~on day_quests =
  Volunteers.fold volunteers ~init:0 ~f:(fun acc v ->
      (* Don't count the load of manually assigned volunteers *)
      if v.initial.manually_assigned then acc
      else
        let v_load =
          match theoretical_load infos ~of_:v ~on day_quests with
          | `Fixed load | `Flexible load -> load
        in
        acc + Duration.to_minutes v_load)

let adjusted_load_minutes infos ?(unit = `Minutes) volunteers volunteer day
    day_quests =
  let volunteer_theoretical_load =
    (* Manually assigned volunteers are not adjusted *)
    theoretical_load infos ~of_:volunteer ~on:day day_quests
  in
  match (volunteer.initial.manually_assigned, volunteer_theoretical_load) with
  | true, (`Fixed load | `Flexible load) | false, `Fixed load ->
      Duration.to_minutes load |> Float.of_int |> Quest.minutes_conv ~unit
      |> Float.round |> Float.to_int
  | false, `Flexible load ->
      let volunteer_theoretical_load = Duration.to_minutes load in
      (* Available time on that day. Not counting manually assigned volunteers *)
      let total_theoretical_load =
        total_theoretical_load infos volunteers ~on:day day_quests
      in
      (* Quest time, not counting quests assigned to manually assigned volunteers *)
      let quests_time = total_quests_time day_quests |> Duration.to_minutes in
      let adjustement_coef =
        Float.(
          of_int volunteer_theoretical_load / of_int total_theoretical_load)
      in
      let adjusted_m = Float.(adjustement_coef * of_int quests_time) in
      let adjusted = Quest.minutes_conv ~unit adjusted_m in
      Logs.debug (fun m ->
          m "%s on %s load: %i / %i = %f   ... * %i = %f" volunteer.name
            (Date.to_string day) volunteer_theoretical_load
            total_theoretical_load adjustement_coef quests_time adjusted);
      Float.to_int adjusted
