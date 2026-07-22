open Data_repr.Rich
open Data_repr.Normal
open! Lunar_jsont
open Result.Infix

let result_of_bool err b =
  if b then Ok () else Error (String.concat ~sep:" " err)

let v_is_manually_assigned_to_q v (q : Quest.t) =
  Volunteers.mem v q.Quest.assigned_volunteers

(** Check specialist skills and banned quests types *)
let v_can_do_task (v : Volunteer.t) ?q (t : Task_type.t) =
  let name = Option.map_or ~default:t.name (fun q -> q.Quest.name) q in
  let* _has_correct_speciality =
    if t.specialist_only then
      let skills = CCRAL.to_list v.initial.proficiencies in
      List.mem ~eq:Task_type.equal t skills
      |> result_of_bool [ v.name; "does not have the required skill for"; name ]
    else Ok ()
  in
  (not (Task_type.Set.mem t v.forbidden_tasks))
  |> result_of_bool [ v.name; "does not have the right to do"; name ]

let v_can_do_quest_task v (q : Quest.t) =
  Option.map_or ~default:(Ok ()) (v_can_do_task v ~q) q.initial.task_type

(** Checks arrival and departure times and other unavailability slots *)
let v_is_available_during (v : Volunteer.t) ?q (t : Time_slot.t) =
  let name =
    Option.map_lazy (fun () -> Time_slot.to_string t) (fun q -> q.Quest.name) q
  in
  let* _arrived =
    match v.initial.arrival with
    | None -> Ok ()
    | Some arrival ->
        Zoned_datetime.(arrival <= t.start)
        |> result_of_bool [ v.name; "is not here for"; name ]
  in
  let* _didnt_leave =
    match v.initial.departure with
    | None -> Ok ()
    | Some departure ->
        Zoned_datetime.(t.start < departure)
        |> result_of_bool [ v.name; "is not here for"; name ]
  in
  let has_overlapping_unavailability () =
    List.exists v.unavailabilities ~f:(Time_slot.overlaps t)
  in
  (not (has_overlapping_unavailability ()))
  |> result_of_bool [ v.name; "is not available for"; name ]

let v_has_no_ennemies_assigned_to_q (v : Volunteer.t) (q : Quest.t) =
  match
    Volunteers.find_first_opt
      (fun v' -> List.mem ~eq:id_equal v'.initial.id v.initial.ennemis)
      q.assigned_volunteers
  with
  | None -> Ok ()
  | Some v' ->
      result_of_bool [ v.name; "cannot do"; q.name; "with"; v'.name ] false

let v_is_not_assigned_to_an_overlapping_q infos all_quests v q =
  match
    Quests.find_first_opt
      (fun q' ->
        (not (String.equal q.Quest.id q'.id))
        && Quest.overlaps infos q q'
        && v_is_manually_assigned_to_q v q')
      all_quests
  with
  | None -> Ok ()
  | Some q' ->
      result_of_bool [ v.name; "cannot do both"; q.name; "and"; q'.name ] false

let v_can_do_q_res infos all_quests v q =
  if v_is_manually_assigned_to_q v q then Ok ()
  else begin
    let* () =
      (not v.initial.manually_assigned)
      |> result_of_bool [ v.name; "can only be manually assigned to"; q.name ]
    in
    let* () = v_can_do_quest_task v q in
    let* () = v_is_available_during v (Quest.real_slot q) in
    let* () = v_has_no_ennemies_assigned_to_q v q in
    v_is_not_assigned_to_an_overlapping_q infos all_quests v q
  end

let v_can_do_q i aq v q =
  match v_can_do_q_res i aq v q with Ok () -> true | Error _ -> false
