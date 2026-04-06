open Std
open Types
open Lunar_jsont

module Time_slot = struct
  type t = { start : Datetime.t; duration : Duration.t } [@@deriving jsont]

  let end_ t = Datetime.(t.start + t.duration)
end

module Quest = struct
  type t = {
    id : Uuidm.t;
    initial : Quest.t;
    name : string;
    slot : Time_slot.t;
  }

  let equal q1 q2 = Uuidm.equal q1.id q2.id

  module Set = Set.Make (struct
    type nonrec t = t

    let compare q1 q2 =
      if equal q1 q2 then 0 else Datetime.compare q1.slot.start q2.slot.start
  end)

  (** Check if two quests are overlapping. If they are in separate places they
      must be separated by at least [Options.minimum_transfer_time]. *)
  let overlaps { Options.minimum_transfer_time; _ } (q1 : t) (q2 : t) =
    let same_place = uuid_equal q1.initial.place.id q2.initial.place.id in
    let q1_start, q1_end = (q1.slot.start, Time_slot.end_ q1.slot) in
    let q2_start, q2_end = (q2.slot.start, Time_slot.end_ q2.slot) in
    let q2_start, q2_end =
      if same_place then (q2_start, q2_end)
      else
        ( Datetime.(q2_start - minimum_transfer_time),
          Datetime.(q2_end + minimum_transfer_time) )
    in
    not Datetime.(q1_end <= q2_start || q1_start >= q2_end)

  (* Returns the set of quests from [qs] overlapping with [q].
     Note that this set might contain [q] itself if [q] ∈ [qs]. *)
  let overlaps_with options q qs =
    List.fold_left qs ~init:Set.empty ~f:(fun acc q' ->
        if overlaps options q q' then Set.add q' acc else acc)

  (** Generate sub-quests depending of the recurrence and the task type's
      divisibility. *)
  let normalize { Event_infos.kind = Finite { start_date; end_date }; _ }
      (q : Quest.t) =
    let spec = q.slot in
    let start_time = spec.start in
    let duration = spec.duration in
    let all_dates =
      match spec.recurrence with
      | On dates -> dates
      | Daily ->
          Date.Range.(
            make ~first:start_date ~last:end_date
            |> to_list ~include_boundaries:true ~iterator:iterator_day)
      | Weekly _ -> failwith "Not implemented"
    in
    List.map all_dates ~f:(fun date ->
        let start = Datetime.from date start_time in
        let id = new_random_uuid_v4 () in
        let name = Format.sprintf "%s_%s" q.name (Date.to_string date) in
        { id; initial = q; name; slot = { Time_slot.start; duration } })
end
