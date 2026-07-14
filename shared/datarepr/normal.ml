open Rich
open Lunar_jsont

module Time_slot = struct
  type t = { start : Zoned_datetime.t;  (** UTC *) duration : Duration.t }
  [@@deriving jsont]

  let dummy = { start = Zoned_datetime.epoch (); duration = Duration.zero }
  let end_ t = Zoned_datetime.(t.start + t.duration)

  let to_string t =
    let start = Zoned_datetime.local_time t.start in
    let end_ = Zoned_datetime.local_time (end_ t) in
    let start_h = Time.hour start |> Utils.lpad ~size:2 in
    let start_m =
      Time.minute start |> function 0 -> "" | m -> Utils.lpad ~size:2 m
    in
    let end_h = Time.hour end_ |> Utils.lpad ~size:2 in
    let end_m =
      Time.minute end_ |> function 0 -> "" | m -> Utils.lpad ~size:2 m
    in
    start_h ^ "h" ^ start_m ^ " / " ^ end_h ^ "h" ^ end_m

  let overlaps t1 t2 =
    let t1_start, t1_end = (t1.start, end_ t1) in
    let t2_start, t2_end = (t2.start, end_ t2) in
    not Zoned_datetime.(t1_end <= t2_start || t1_start >= t2_end)
end

module Volunteer = struct
  module T = struct
    type t = {
      id : string;
      name : string;
      initial : Volunteer.t;
      forbidden_tasks : Task_type.Set.t;
      wanted_tasks : Task_type.Set.t;
      unwanted_tasks : Task_type.Set.t;
      unavailabilities : Time_slot.t list;
      preferences : (int * Time_slot.t) list;
    }
    [@@deriving jsont]

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
      wanted_tasks = Task_type.Set.empty;
      unwanted_tasks = Task_type.Set.empty;
      unavailabilities = [];
      preferences = [];
    }

  module Set = struct
    include Set.Make_jsont (T)

    let find_by_id id t = find { dummy with id } t
  end

  module Map = struct
    include Map.Make_jsont (T)

    let find_by_id id = find { dummy with id }
  end

  let to_string t = "V: [" ^ t.id ^ "] " ^ t.name

  let is_on_site_at time t =
    match (t.initial.arrival, t.initial.departure) with
    | Some arrival, _ when Zoned_datetime.(time < arrival) -> false
    | _, Some departure when Zoned_datetime.(departure < time) -> false
    | _ -> true

  let is_available_at time t =
    is_on_site_at time t
    && not
         (List.exists t.unavailabilities ~f:(fun (slot : Time_slot.t) ->
              Zoned_datetime.(slot.start <= time && time < Time_slot.end_ slot)))
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
    [@@deriving jsont]

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
    include Set.Make_jsont (T)

    let find_by_id id t = find { dummy with id } t
  end

  module Map = struct
    include Map.Make_jsont (T)

    let find_by_id id t = find { dummy with id } t
  end

  let is_free t = Quest.is_free t.initial

  (** Returns the complete slot taking into account preparation time and rest
      time. *)
  let real_slot t =
    let start, duration =
      match t.initial.task_type with
      | Some { required_time_before = Some offset; _ } ->
          ( Zoned_datetime.(t.slot.start - offset),
            Duration.(t.slot.duration + offset) )
      | _ -> (t.slot.start, t.slot.duration)
    in
    let duration =
      match t.initial.task_type with
      | Some { required_time_after = Some offset; _ } ->
          Duration.(duration + offset)
      | _ -> duration
    in
    { Time_slot.start; duration }

  let minutes_conv ~unit t =
    match unit with
    | `Minutes -> t
    | `Five_minutes -> Float.(t / 5. |> round)
    | `Fifteen_minutes -> Float.(t / 15. |> round)
    | `N_minutes n -> Float.(t / of_int n |> round)

  (** The "real" quest duration. Free quests last 0 minutes. *)
  let real_duration ?(unit = `Minutes) q =
    let duration_m =
      if is_free q then 0
      else Duration.to_minutes q.slot.duration (* Resolution 15 ? *)
    in
    Float.to_int (minutes_conv ~unit (Float.of_int duration_m))

  (** Returns the total time spent for a quest (duration * number of
      volunteers). Free quests last 0. *)
  let weighted_duration ?(unit = `Minutes) q =
    (* TODO We should not count quest time for assigned volunteers that are manually
     assigned. If their time is 0 or the length of the quest, applying the
     proportional coefficient will biased them. *)
    let duration = real_duration ~unit q in
    duration * q.initial.required_volunteers

  (** Check if two quests are overlapping. If they are in separate places they
      must be separated by at least [Options.minimum_transfer_time]. If the
      quest type(s) require preparation or rest time these are also taken into
      account. *)
  let overlaps { Event_infos.minimum_transfer_time; _ } (q1 : t) (q2 : t) =
    let same_place =
      match (q1.initial.place, q2.initial.place) with
      | Some p1, Some p2 -> Rich.Place.equal p1 p2
      | _, _ -> false
    in
    let q1_slot, q2_slot = (real_slot q1, real_slot q2) in
    let q1_start, q1_end = (q1_slot.start, Time_slot.end_ q1_slot) in
    let q2_start, q2_end = (q2_slot.start, Time_slot.end_ q2_slot) in
    let q2_start, q2_end =
      if same_place then (q2_start, q2_end)
      else
        ( Zoned_datetime.(q2_start - minimum_transfer_time),
          Zoned_datetime.(q2_end + minimum_transfer_time) )
    in
    not Zoned_datetime.(q1_end <= q2_start || q1_start >= q2_end)

  (** Returns the set of quests from [qs] overlapping with [q]. Note that this
      set might contain [q] itself if [q] ∈ [qs]. *)
  let overlaps_with infos q qs = Set.filter (overlaps infos q) qs

  let is_manually_assigned_to (v : Volunteer.t) t =
    Volunteers.mem v t.assigned_volunteers
end

module Quests = Quest.Set

module Quests_group = struct
  type t = {
    name : string;
    quests : Quests.t;
    quests_constraint : Quests_group.quests_constraint;
  }
  [@@deriving jsont]

  let to_string { name; quests; quests_constraint } =
    let cstr = Quests_group.string_of_quests_constraint quests_constraint in
    let quests =
      Quests.to_list_map quests ~f:(fun q ->
          q.name ^ "(end: "
          ^ Zoned_datetime.to_string (Time_slot.end_ q.slot)
          ^ ")")
    in
    "Group " ^ name ^ " [" ^ cstr ^ "]: " ^ String.concat ~sep:"; " quests
end

let to_event_local_date (infos : Event_infos.t) datetime =
  Zoned_datetime.(datetime - Time.to_duration infos.day_start_local)
  |> Zoned_datetime.local_date

let quests_by_day (infos : Event_infos.t) quests =
  Quests.fold quests ~init:Date.Map.empty ~f:(fun acc q ->
      Date.Map.update
        (to_event_local_date infos q.slot.start)
        (function
          | None -> Some (Quests.singleton q)
          | Some quests -> Some (Quests.add q quests))
        acc)
