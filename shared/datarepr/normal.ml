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

let expand_time_spec
    { Event_infos.kind = Finite { start_date; end_date }; timezone = tz; _ }
    (spec : Time_spec.t) =
  let first_day =
    match spec.first_day with
    | None -> start_date
    | Some date -> Date.max start_date date
  in
  let last_day =
    match spec.last_day with
    | None -> end_date
    | Some date -> Date.min end_date date
  in
  let start_time = spec.start in
  let duration = spec.duration in
  let all_dates =
    let all_days_in_range () =
      Date.Range.(
        make ~first:first_day ~last:last_day
        |> to_list ~include_boundaries:true ~iterator:iterator_day)
    in
    match spec.recurrence with
    | On dates -> dates
    | Daily -> all_days_in_range ()
    | Weekly weekdays ->
        let range = all_days_in_range () in
        List.filter range ~f:(fun d ->
            let wd = Date.day_of_week d in
            Weekday.Set.mem wd weekdays)
  in
  List.map all_dates ~f:(fun date ->
      let start = Zoned_datetime.(from ~tz date start_time |> to_utc) in
      { Time_slot.start; duration })

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

  let normalize event_infos (v : Volunteer.t) =
    let name =
      match v.public_name with
      | Some public_name -> public_name
      | None -> v.name
    in
    let forbidden_tasks =
      Task_type.Set.of_list (CCRAL.to_list v.forbidden_tasks)
    in
    let unavailabilities, preferences =
      CCRAL.fold v.availabilities ~x:([], [])
        ~f:(fun (u_acc, p_acc) { Availability.status; slot } ->
          let slots = expand_time_spec event_infos slot in
          match status with
          | Unavailable -> (List.rev_append slots u_acc, p_acc)
          | Available pref ->
              (u_acc, List.rev_append (List.map slots ~f:(Pair.make pref)) p_acc))
    in
    {
      id = id_to_string v.id;
      name;
      initial = v;
      forbidden_tasks;
      unavailabilities;
      preferences;
    }

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
    let same_place = Rich.id_equal q1.initial.place.id q2.initial.place.id in
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

  (** Generate sub-quests depending of the recurrence and the task type's
      divisibility. *)
  let normalize event_infos vs (q : Quest.t) =
    let assigned_volunteers =
      CCRAL.fold q.assigned_volunteers ~x:Volunteer.Set.empty
        ~f:(fun acc (v : Rich.Volunteer.t) ->
          Volunteer.Set.add
            (Volunteer.Set.find_by_id (id_to_string v.id) vs)
            acc)
    in
    let slots = expand_time_spec event_infos q.slot in
    List.mapi slots ~f:(fun i (slot : Time_slot.t) ->
        let id = Printf.sprintf "%s_%i" (Rich.id_to_string q.id) i in
        let name =
          Printf.sprintf "%s_%s" q.name (Datetime.to_string slot.start)
        in
        { id; initial = q; name; slot; assigned_volunteers })
end

module Quests = Quest.Set

type data = { volunteers : Volunteers.t; quests : Quests.t }
