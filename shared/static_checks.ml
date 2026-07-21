open Data_repr.Rich
open Data_repr.Normal
open! Lunar_jsont

let v_is_manually_assigned_to_q v (q : Quest.t) =
  Volunteers.mem v q.Quest.assigned_volunteers

let v_can_do_task (v : Volunteer.t) (t : Task_type.t) =
  let has_correct_speciality =
    if t.specialist_only then
      let skills = CCRAL.to_list v.initial.proficiencies in
      List.mem ~eq:Task_type.equal t skills
    else true
  in
  has_correct_speciality && not (Task_type.Set.mem t v.forbidden_tasks)

let v_can_do_quest_task v (q : Quest.t) =
  Option.map_or ~default:true (v_can_do_task v) q.initial.task_type

let v_is_available_durring (v : Volunteer.t) (t : Time_slot.t) =
  let arrived =
    match v.initial.arrival with
    | None -> true
    | Some arrival -> Zoned_datetime.(arrival <= t.start)
  in
  let didnt_leave =
    match v.initial.departure with
    | None -> true
    | Some departure -> Zoned_datetime.(t.start < departure)
  in
  let has_overlapping_unavailability () =
    List.exists v.unavailabilities ~f:(Time_slot.overlaps t)
  in
  arrived && didnt_leave && not (has_overlapping_unavailability ())

let v_has_no_ennemies_assigned_to_q (v : Volunteer.t) (q : Quest.t) =
  not
  @@ Volunteers.exists
       (fun v' -> List.mem ~eq:id_equal v'.initial.id v.initial.ennemis)
       q.assigned_volunteers

let v_is_not_assigned_to_an_overlapping_q infos all_quests v q =
  Quests.exists
    (fun q' ->
      (not (String.equal q.Quest.id q'.id))
      && Quest.overlaps infos q q'
      && v_is_manually_assigned_to_q v q')
    all_quests

let v_can_do_q infos all_quests v q =
  v_is_manually_assigned_to_q v q
  || begin
    (not v.initial.manually_assigned)
    && v_can_do_quest_task v q
    && v_is_available_durring v (Quest.real_slot q)
    && v_has_no_ennemies_assigned_to_q v q
    && v_is_not_assigned_to_an_overlapping_q infos all_quests v q
  end
