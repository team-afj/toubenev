open Lunar_jsont
module Timezones = Timezones

(* TODO the [edit] types are meant for use with a future web-ui supporting
   OT-style cooperative edition. Right now they are not exposed nor used.

   Also the existing "edits" would require most record fields to be mutable to
   work properly, because objects are cross-referenced, this is not the case
   right now and need to be thinked about carrefully. *)

(* TODO we should probaly have a functor for "Stores" and rely primarily on
   cross-references *)

module type Editable = sig
  type t
  type edit

  val edit_jsont : edit Jsont.t
  val apply_edit : edit -> t -> t (* result *)
end

module type Jsontable = sig
  type t

  val jsont : t Jsont.t
end

module type S = sig
  type t

  include Editable with type t := t
  include Jsontable with type t := t
end

module Random_access_list (X : S) : S with type t = X.t CCRAL.t = struct
  type t = X.t CCRAL.t

  let jsont =
    Jsont.array X.jsont |> Jsont.map ~dec:CCRAL.of_array ~enc:CCRAL.to_array

  type edit =
    | Insert_at of (int * X.t)
    | Remove of int
    | Update of (int * X.edit)
  [@@deriving jsont]

  let apply_edit edit t =
    match edit with
    | Insert_at (i, v) ->
        let before, after = CCRAL.take_drop i t in
        CCRAL.append before (CCRAL.cons v after)
    | Remove i -> CCRAL.remove t i
    | Update (i, edit) ->
        Option.fold
          (fun t v -> X.apply_edit edit v |> CCRAL.set t i)
          t (CCRAL.get t i)
end

type _ id = string

let id_equal u1 u2 = String.equal u1 u2
let id_jsont _ = Jsont.string
let make_id () = new_random_uuid_v4 () |> Uuidm.to_string
let id_to_string t = t
let id_to_int t = Int.of_string_exn t
let id_of_int t = Int.to_string t

module Event_infos = struct
  type kind =
    (* TODO Weekly | Monthly *)
    | Finite of { start_date : Date.t; end_date : Date.t }
  [@@deriving jsont]

  type t = {
    name : string;
    kind : kind;
    timezone : Timezone.t;
    day_start_utc : Time.t;
    minimum_transfer_time : Duration.t;
    daily_break_duration : Duration.t;
  }
  [@@deriving jsont]
end

module Options = struct
  type t = {
    min_quest_duration : Duration.t;
    max_quest_duration : Duration.t;
    friendship_bonus : int;
    desired_time_bonus : int;
    undesired_time_malus : int;
    desired_quest_bonus : int;
    undesired_quest_bonus : int;
    large_amplitude_malus : int;
    daily_equilibrium_malus : int;
    event_equilibrium_malus : int;
  }
  [@@deriving jsont]

  let default =
    {
      min_quest_duration = Duration.from_minutes 45;
      max_quest_duration = Duration.from_minutes 120;
      friendship_bonus = 1;
      desired_time_bonus = 1;
      undesired_time_malus = 1;
      desired_quest_bonus = 1;
      undesired_quest_bonus = 1;
      large_amplitude_malus = 1;
      daily_equilibrium_malus = 10;
      event_equilibrium_malus = 5;
    }
end

module Place = struct
  module T = struct
    type t = {
      id : t id;
      slug : string;
      name : string;
      description : string option;
    }
    [@@deriving jsont]

    let equal p1 p2 = id_equal p1.id p2.id
    let compare t1 t2 = String.compare (id_to_string t1.id) (id_to_string t2.id)
  end

  include T
  module Map = Map.Make_jsont (T)

  let dummy = { id = ""; slug = ""; name = ""; description = None }

  type edit =
    | New_slug of string
    | New_name of string
    | New_description of string option
  [@@deriving jsont]

  let apply_edit edit t =
    match edit with
    | New_slug slug -> { t with slug }
    | New_name name -> { t with name }
    | New_description description -> { t with description }

  let make ?id ~slug ~name ?description () =
    let id = Option.get_lazy make_id id in
    { id; slug; name; description }
end

module Places = Random_access_list (Place)

module Task_type = struct
  type task_sharing = Not_necessarily | At_least_once | In_equal_proportion
  [@@deriving jsont]

  module T = struct
    type t = {
      id : t id;
      slug : string;
      name : string;
      description : string option;
      everyone_should_do_it : task_sharing;
      specialist_only : bool;
      divisible : bool;
      free : bool;
    }
    [@@deriving jsont]

    let equal t1 t2 = String.equal (id_to_string t1.id) (id_to_string t2.id)
    let compare t1 t2 = String.compare (id_to_string t1.id) (id_to_string t2.id)
  end

  include T
  module Set = Set.Make_jsont (T)

  let dummy =
    {
      id = "";
      slug = "";
      name = "";
      description = None;
      everyone_should_do_it = Not_necessarily;
      specialist_only = false;
      divisible = false;
      free = false;
    }

  type edit =
    | New_slug of string
    | New_name of string
    | New_description of string option
    | New_specialist_only of bool
    | New_divisible of bool
  [@@deriving jsont]

  let apply_edit edit t =
    match edit with
    | New_slug slug -> { t with slug }
    | New_name name -> { t with name }
    | New_description description -> { t with description }
    | New_specialist_only specialist_only -> { t with specialist_only }
    | New_divisible divisible -> { t with divisible }

  let make ?id ~slug ~name ?description
      ?(everyone_should_do_it = Not_necessarily) ~specialist_only ~divisible
      ~free () =
    let id = Option.get_lazy make_id id in
    {
      id;
      slug;
      name;
      description;
      everyone_should_do_it;
      specialist_only;
      divisible;
      free;
    }
end

module Task_types = Random_access_list (Task_type)

module Time_spec = struct
  type recurrence = Daily | Weekly of Weekday.Set.t | On of Date.t list
  [@@deriving jsont]

  let string_of_recurrence = function
    | Daily -> "daily"
    | Weekly weekdays ->
        let weekdays =
          Weekday.Set.to_list weekdays
          |> List.map ~f:Weekday.to_string
          |> String.concat ~sep:", "
        in
        Printf.sprintf "weekly (%s)" weekdays
    | On dates ->
        let dates =
          List.map dates ~f:Date.to_string |> String.concat ~sep:", "
        in
        Printf.sprintf "on %s" dates

  type t = {
    recurrence : recurrence;
    start : Time.t;
    duration : Duration.t;
    first_day : Date.t option;
    last_day : Date.t option;
  }
  [@@deriving jsont]

  type edit =
    | New_recurrence of recurrence
    | New_start of Time.t
    | New_duration of Duration.t
  [@@deriving jsont]

  let apply_edit edit t =
    match edit with
    | New_recurrence recurrence -> { t with recurrence }
    | New_start start -> { t with start }
    | New_duration duration -> { t with duration }

  let make recurrence ?first_day ?last_day start duration =
    { recurrence; first_day; last_day; start; duration }

  let to_string (t : t) =
    let recurrence = string_of_recurrence t.recurrence in
    let start = Time.to_string t.start in
    let duration =
      let hours, minutes, seconds = Duration.hms t.duration in
      if seconds = 0 then Printf.sprintf "%02dh%02dm" hours minutes
      else Printf.sprintf "%02dh%02dm%02ds" hours minutes seconds
    in
    let bounds =
      match (t.first_day, t.last_day) with
      | None, None -> ""
      | Some first_day, None ->
          Printf.sprintf " from %s" (Date.to_string first_day)
      | None, Some last_day ->
          Printf.sprintf " until %s" (Date.to_string last_day)
      | Some first_day, Some last_day ->
          Printf.sprintf " from %s to %s" (Date.to_string first_day)
            (Date.to_string last_day)
    in
    Printf.sprintf "%s at %s for %s%s" recurrence start duration bounds
end

module Time_specs = Random_access_list (Time_spec)

module Availability = struct
  type status = Unavailable | Available of int [@@deriving jsont]
  type t = { status : status; slot : Time_spec.t } [@@deriving jsont]

  type edit =
    | New_status of status
    | New_slot of Time_spec.t
    | Slot_update of Time_spec.edit
  [@@deriving jsont]

  let apply_edit edit t =
    match edit with
    | New_status status -> { t with status }
    | New_slot slot -> { t with slot }
    | Slot_update edit -> { t with slot = Time_spec.apply_edit edit t.slot }
end

module Availabilities = Random_access_list (Availability)

module Volunteer = struct
  type t = {
    id : t id;
    public_name : string option;
    name : string;
    daily_workload : Duration.t;
    manually_assigned : bool;
    availabilities : Availabilities.t;
    arrival : Zoned_datetime.t option;
    departure : Zoned_datetime.t option;
    mutable friends : t id list;
    mutable ennemis : t id list;
    proficiencies : Task_types.t;
    forbidden_tasks : Task_types.t;
    forbidden_places : Places.t;
  }
  [@@deriving jsont]

  let dummy =
    {
      id = "";
      public_name = None;
      name = "";
      daily_workload = Duration.zero;
      manually_assigned = false;
      availabilities = CCRAL.empty;
      arrival = None;
      departure = None;
      friends = [];
      ennemis = [];
      proficiencies = CCRAL.empty;
      forbidden_tasks = CCRAL.empty;
      forbidden_places = CCRAL.empty;
    }

  type edit =
    | New_public_name of string option
    | New_name of string
    | New_daily_workload of Duration.t
    | Update_availabilities of Availabilities.edit
    | New_arrival of Zoned_datetime.t option
    | New_departure of Zoned_datetime.t option
    | New_friends of t id list
    | New_ennemis of t id list
  [@@deriving jsont]

  let apply_edit (edit : edit) (t : t) : t =
    match edit with
    | New_public_name public_name -> { t with public_name }
    | New_name name -> { t with name }
    | New_daily_workload daily_workload -> { t with daily_workload }
    | Update_availabilities edit ->
        {
          t with
          availabilities = Availabilities.apply_edit edit t.availabilities;
        }
    | New_arrival arrival -> { t with arrival }
    | New_departure departure -> { t with departure }
    | New_friends (friends : t id list) -> { t with friends }
    | New_ennemis (ennemis : t id list) -> { t with ennemis }

  let make ?id ?(friends = []) ?(ennemis = []) ?(proficiencies = CCRAL.empty)
      ?(forbidden_tasks = CCRAL.empty) ?(forbidden_places = CCRAL.empty)
      ?(availabilities = CCRAL.empty) ?arrival ?departure
      ?(manually_assigned = false) ~daily_workload ~name ?public_name () =
    let id = Option.get_lazy make_id id in
    {
      id;
      public_name;
      name;
      daily_workload;
      manually_assigned;
      availabilities;
      arrival;
      departure;
      proficiencies;
      friends;
      ennemis;
      forbidden_tasks;
      forbidden_places;
    }

  let set_friends t fs = t.friends <- fs
  let set_ennemis t fs = t.ennemis <- fs
end

module Volunteers = Random_access_list (Volunteer)

module Quest = struct
  type t = {
    id : t id;
    name : string;
    description : string option;
    task_type : Task_type.t option;
    (* group: Quest_group.t TODO *)
    place : Place.t option;
    slot : Time_spec.t;
    required_volunteers : int;
    assigned_volunteers : Volunteers.t;
  }
  [@@deriving jsont]

  let dummy =
    {
      id = "";
      name = "";
      description = None;
      task_type = None;
      place = None;
      slot = Time_spec.make Daily Time.noon Duration.zero;
      required_volunteers = 0;
      assigned_volunteers = CCRAL.empty;
    }

  type edit =
    | New_name of string
    | New_description of string option
    | New_task_type of Task_type.t option
    | New_place of Place.t option
    | New_slot of Time_spec.t
    | Update_slot of Time_spec.edit
    | New_required_volunteers of int
  [@@deriving jsont]

  let apply_edit edit t =
    match edit with
    | New_name name -> { t with name }
    | New_description description -> { t with description }
    | New_task_type task_type -> { t with task_type }
    | New_place place -> { t with place }
    | New_slot slot -> { t with slot }
    | Update_slot edit -> { t with slot = Time_spec.apply_edit edit t.slot }
    | New_required_volunteers required_volunteers ->
        { t with required_volunteers }

  let make ?id ~name ?description ?task_type ?place ~slot ~required_volunteers
      ?(assigned_volunteers = CCRAL.empty) () =
    let id = Option.get_lazy make_id id in
    {
      id;
      name;
      description;
      task_type;
      place;
      slot;
      required_volunteers;
      assigned_volunteers;
    }

  let is_free (t : t) =
    Option.map_or ~default:false (fun qt -> qt.Task_type.free) t.task_type
end

module Quests = Random_access_list (Quest)

module Planning = struct
  type t = {
    options : Options.t;
    infos : Event_infos.t;
    places : Places.t;
    task_types : Task_types.t;
    volunteers : Volunteers.t;
    quests : Quests.t;
  }
  [@@deriving jsont]

  type edit =
    | Places of Places.edit
    | Task_types of Task_types.edit
    | Volunteers of Volunteers.edit
    | Quests of Quests.edit
  [@@deriving jsont]

  let apply_edit edit t =
    match edit with
    | Places edit -> { t with places = Places.apply_edit edit t.places }
    | Task_types edit ->
        { t with task_types = Task_types.apply_edit edit t.task_types }
    | Volunteers edit ->
        { t with volunteers = Volunteers.apply_edit edit t.volunteers }
    | Quests edit -> { t with quests = Quests.apply_edit edit t.quests }
end
