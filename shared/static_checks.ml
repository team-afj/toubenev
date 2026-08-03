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

let max_doable can_do (volunteer : Volunteer.t) quests =
  let doable_quests =
    Quests.filter (can_do volunteer) quests
    |> Quests.to_list
    |> List.sort ~cmp:(fun q1 q2 ->
        Zoned_datetime.compare
          (Time_slot.end_ (Quest.real_slot q1))
          (Time_slot.end_ (Quest.real_slot q2)))
  in
  let doable_quests =
    List.filter ~f:(fun q -> not (Quest.is_free q)) doable_quests
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

type with_cache = {
  can_do_res : Volunteers.elt -> Quests.elt -> (unit, string) result;
  can_do : Volunteers.elt -> Quests.elt -> bool;
  max_doable : ?key:string -> Volunteers.elt -> Quests.t -> Duration.t;
}

let make infos all_quests () =
  let can_do_cache = Hashtbl.create 1024 in
  let max_doable_cache = Hashtbl.create 1024 in
  let can_do_res (v : Volunteer.t) (q : Quest.t) =
    match Hashtbl.find_opt can_do_cache (v.id, q.id) with
    | Some res -> res
    | None ->
        let res = v_can_do_q_res infos all_quests v q in
        Hashtbl.add can_do_cache (v.id, q.id) res;
        res
  in
  let can_do v q =
    match can_do_res v q with Ok () -> true | Error _ -> false
  in
  let max_doable ?key (v : Volunteer.t) (qs : Quests.t) =
    let key =
      Option.get_lazy
        (fun () ->
          let buf = Buffer.create (Quests.cardinal qs) in
          Quests.iter qs ~f:(fun q -> Buffer.add_string buf q.id);
          Buffer.contents buf)
        key
    in
    let key = (v.id, key) in
    match Hashtbl.find_opt max_doable_cache key with
    | Some res -> res
    | None ->
        let res = max_doable can_do v qs in
        Hashtbl.add max_doable_cache key res;
        res
  in
  { can_do_res; can_do; max_doable }
