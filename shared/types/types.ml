open Lunar_jsont

(* TODO the [edit] types are meant for use with a future web-ui supporting
   OT-style cooperative edition. Right now they are not exposed nor used. *)

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

module Place = struct
  type t = {
    id : int;
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

  let store : t Dynarray.t = Dynarray.create ()

  let make ~slug ~name ?description () =
    let id = Dynarray.length store in
    let v : t = { id; slug; name; description } in
    Dynarray.add_last store v;
    v
end

module Places = Random_access_list (Place)

module Task_type = struct
  type t = {
    id : int;
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

  let store : t Dynarray.t = Dynarray.create ()

  let make ~slug ~name ?description ~specialist_only ~divisible () =
    let id = Dynarray.length store in
    let v : t = { id; slug; name; description; specialist_only; divisible } in
    Dynarray.add_last store v;
    v
end

module Task_types = Random_access_list (Task_type)

module Time_slot = struct
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

module Time_slots = Random_access_list (Time_slot)

module Availability = struct
  type status = Unavailable | Available of int [@@deriving jsont]
  type t = { status : status; slot : Time_slot.t } [@@deriving jsont]

  type edit =
    | New_status of status
    | New_slot of Time_slot.t
    | Slot_update of Time_slot.edit
  [@@deriving jsont]

  let apply_edit edit t =
    match edit with
    | New_status status -> { t with status }
    | New_slot slot -> { t with slot }
    | Slot_update edit -> { t with slot = Time_slot.apply_edit edit t.slot }
end

module Availabilities = Random_access_list (Availability)

module Volunteer = struct
  type t = {
    id : int;
    public_name : string option;
    name : string;
    daily_workload : int;
    availabilities : Availabilities.t;
    arrival : Datetime.t option;
    departure : Datetime.t option;
    proficiencies : int list;
    friends : int list;
    ennemis : int list;
    forbidden_tasks : int list;
    forbidden_places : int list;
  }
  [@@deriving jsont]

  type edit =
    | New_public_name of string option
    | New_name of string
    | New_daily_workload of int
    | Update_availabilities of Availabilities.edit
    | New_arrival of Datetime.t option
    | New_departure of Datetime.t option
    | New_friends of int list
    | New_ennemis of int list
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
    | New_friends (friends : int list) -> { t with friends }
    | New_ennemis (ennemis : int list) -> { t with ennemis }

  let store : t Dynarray.t = Dynarray.create ()

  let make ?id ?(friends = []) ?(ennemis = []) ?(proficiencies = [])
      ?(forbidden_tasks = []) ?(forbidden_places = [])
      ?(availabilities = CCRAL.empty) ~name () =
    let id = Option.get_lazy (fun () -> Dynarray.length store) id in
    let v : t =
      {
        id;
        public_name = None;
        name;
        daily_workload = 0;
        availabilities;
        arrival = None;
        departure = None;
        proficiencies;
        friends;
        ennemis;
        forbidden_tasks;
        forbidden_places;
      }
    in
    Dynarray.add_last store v;
    v
end

module Volunteers = Random_access_list (Volunteer)

module Quest = struct
  type t = {
    id : int;
    slug : string;
    name : string;
    description : string option;
    task_type : Task_type.t;
    (* group: Quest_group.t TODO *)
    place : Place.t;
    slot : Time_slot.t;
    required_volunteers : int;
  }
  [@@deriving jsont]

  type edit =
    | New_slug of string
    | New_name of string
    | New_description of string option
    | New_task_type of Task_type.t
    | New_place of Place.t
    | New_slot of Time_slot.t
    | Update_slot of Time_slot.edit
    | New_required_volunteers of int
  [@@deriving jsont]

  let apply_edit edit t =
    match edit with
    | New_slug slug -> { t with slug }
    | New_name name -> { t with name }
    | New_description description -> { t with description }
    | New_task_type task_type -> { t with task_type }
    | New_place place -> { t with place }
    | New_slot slot -> { t with slot }
    | Update_slot edit -> { t with slot = Time_slot.apply_edit edit t.slot }
    | New_required_volunteers required_volunteers ->
        { t with required_volunteers }

  let store : t Dynarray.t = Dynarray.create ()

  let make ~slug ~name ?description ~task_type ~place ~slot ~required_volunteers
      () =
    let id = Dynarray.length store in
    let v : t =
      {
        id;
        slug;
        name;
        description;
        task_type;
        place;
        slot;
        required_volunteers;
      }
    in
    Dynarray.add_last store v;
    v
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
