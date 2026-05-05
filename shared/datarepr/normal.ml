open Rich
open Lunar_jsont

module Time_slot = struct
  type t = { start : Datetime.t;  (** UTC *) duration : Duration.t }
  [@@deriving jsont]

  let dummy = { start = Datetime.epoch; duration = Duration.zero }
  let end_ t = Datetime.(t.start + t.duration)

  let overlaps t1 t2 =
    let t1_start, t1_end = (t1.start, end_ t1) in
    let t2_start, t2_end = (t2.start, end_ t2) in
    not Datetime.(t1_end <= t2_start || t1_start >= t2_end)
end

module Volunteer = struct
  module T = struct
    type t = {
      id : string;
      name : string;
      initial : Volunteer.t;
      forbidden_tasks : Task_type.Set.t;
      unavailabilities : Time_slot.t list;
      preferences : (int * Time_slot.t) list;
    }

    let equal v1 v2 = String.equal v1.id v2.id
    let compare v1 v2 = String.compare v1.id v2.id
  end

  include T

  let dummy =
    {
      id = "";
      name = "";
      initial = Volunteer.dummy;
      forbidden_tasks = Task_type.Set.empty;
      unavailabilities = [];
      preferences = [];
    }

  module Set = struct
    include Set.Make (T)

    let find_by_id id t = find { dummy with id } t
  end

  module Map = struct
    include Map.Make (T)

    let find_by_id id = find { dummy with id }
  end

  let to_string t = "V: [" ^ t.id ^ "] " ^ t.name
end

module Volunteers = Volunteer.Set

module Quest = struct
  module T = struct
    type t = {
      id : string;
      initial : Quest.t;
      name : string;
      slot : Time_slot.t;
      assigned_volunteers : Volunteers.t;
    }

    let equal q1 q2 = String.equal q1.id q2.id
    let compare q1 q2 = String.compare q1.id q2.id
  end

  include T

  let dummy =
    {
      id = "";
      initial = Quest.dummy;
      name = "";
      slot = Time_slot.dummy;
      assigned_volunteers = Volunteers.empty;
    }

  module Set = struct
    include Set.Make (T)

    let find_by_id id t = find { dummy with id } t
  end

  (** Check if two quests are overlapping. If they are in separate places they
      must be separated by at least [Options.minimum_transfer_time]. *)
  let overlaps { Options.minimum_transfer_time; _ } (q1 : t) (q2 : t) =
    let same_place =
      match (q1.initial.place, q2.initial.place) with
      | Some p1, Some p2 -> Rich.Place.equal p1 p2
      | _, _ -> false
    in
    let q1_start, q1_end = (q1.slot.start, Time_slot.end_ q1.slot) in
    let q2_start, q2_end = (q2.slot.start, Time_slot.end_ q2.slot) in
    let q2_start, q2_end =
      if same_place then (q2_start, q2_end)
      else
        ( Datetime.(q2_start - minimum_transfer_time),
          Datetime.(q2_end + minimum_transfer_time) )
    in
    not Datetime.(q1_end <= q2_start || q1_start >= q2_end)

  (** Returns the set of quests from [qs] overlapping with [q]. Note that this
      set might contain [q] itself if [q] ∈ [qs]. *)
  let overlaps_with options q qs = Set.filter (overlaps options q) qs
end

module Quests = Quest.Set

let quests_by_day (infos : Event_infos.t) quests =
  Quests.fold quests ~init:Date.Map.empty ~f:(fun acc q ->
      let offseted =
        (* The day is often offseted in a festival *)
        Datetime.(q.slot.start - Time.to_duration infos.day_start_utc)
      in
      Date.Map.update (Datetime.date offseted)
        (function
          | None -> Some Quests.empty
          | Some quests -> Some (Quests.add q quests))
        acc)

type data = { volunteers : Volunteers.t; quests : Quests.t }
