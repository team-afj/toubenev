open Lunar_jsont

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

type _ uuid = Uuidm.t

let uuid_equal u1 u2 = Uuidm.equal u1 u2

let uuid_jsont _ =
  Jsont.map ~dec:Uuidm.unsafe_of_binary_string ~enc:Uuidm.to_binary_string
    Jsont.string

let make_uuid () = new_random_uuid_v4 ()
let uuid_to_uuidm t = t

module Options = struct
  type t = { minimum_transfer_time : Duration.t } [@@deriving jsont]

  let default = { minimum_transfer_time = Duration.from_minutes 15 }
end

module Place = struct
  type t = {
    id : t uuid;
    slug : string;
    name : string;
    description : string option;
  }
  [@@deriving jsont]

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

  let make ~slug ~name ?description () =
    let id = make_uuid () in
    { id; slug; name; description }
end

module Places = Random_access_list (Place)

module Task_type = struct
  type t = {
    id : t uuid;
    slug : string;
    name : string;
    description : string option;
    specialist_only : bool;
    divisible : bool;
  }
  [@@deriving jsont]

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

  let make ~slug ~name ?description ~specialist_only ~divisible () =
    let id = make_uuid () in
    { id; slug; name; description; specialist_only; divisible }
end

module Task_types = Random_access_list (Task_type)

module Time_spec = struct
  type recurrence = Daily | Weekly of Weekday.t list | On of Date.t list
  [@@deriving jsont]

  type t = { recurrence : recurrence; start : Time.t; duration : Duration.t }
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
    id : t uuid;
    public_name : string option;
    name : string;
    daily_workload : Duration.t;
    manually_assigned : bool;
    availabilities : Availabilities.t;
    arrival : Datetime.t option;
    departure : Datetime.t option;
    friends : t uuid list;
    ennemis : t uuid list;
    proficiencies : Task_types.t;
    forbidden_tasks : Task_types.t;
    forbidden_places : Places.t;
  }
  [@@deriving jsont]

  let dummy =
    {
      id = Uuidm.nil;
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
    | New_arrival of Datetime.t option
    | New_departure of Datetime.t option
    | New_friends of t uuid list
    | New_ennemis of t uuid list
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
    | New_friends (friends : t uuid list) -> { t with friends }
    | New_ennemis (ennemis : t uuid list) -> { t with ennemis }

  let make ?(friends = []) ?(ennemis = []) ?(proficiencies = CCRAL.empty)
      ?(forbidden_tasks = CCRAL.empty) ?(forbidden_places = CCRAL.empty)
      ?(availabilities = CCRAL.empty) ?arrival ?departure
      ?(manually_assigned = false) ~daily_workload ~name ?public_name () =
    let id = make_uuid () in
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
end

module Volunteers = Random_access_list (Volunteer)

module Quest = struct
  type t = {
    id : t uuid;
    name : string;
    description : string option;
    task_type : Task_type.t;
    (* group: Quest_group.t TODO *)
    place : Place.t;
    slot : Time_spec.t;
    required_volunteers : int;
    assigned_volunteers : Volunteers.t;
  }
  [@@deriving jsont]

  type edit =
    | New_name of string
    | New_description of string option
    | New_task_type of Task_type.t
    | New_place of Place.t
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

  let make ~name ?description ~task_type ~place ~slot ~required_volunteers
      ?(assigned_volunteers = CCRAL.empty) () =
    let id = make_uuid () in
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
end

module Quests = Random_access_list (Quest)

module Event_infos = struct
  type kind =
    (* TODO Weekly | Monthly *)
    | Finite of { start_date : Date.t; end_date : Date.t }
  [@@deriving jsont]

  type t = { name : string; kind : kind } [@@deriving jsont]
end

module Planning = struct
  type t = {
    options : Options.t;
    info : Event_infos.t;
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
