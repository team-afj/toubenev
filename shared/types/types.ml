open Lunar_jsont

(* TODO the [edit] types are meant for use with a future web-ui supporting
   OT-style cooperative edition. Right now they are not exposed nor used. *)

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
    places : Place.t list;
  }
  [@@deriving jsont]

  type edit =
    | New_slug of string
    | New_name of string
    | New_description of string option
    | New_places of Place.t list
  [@@deriving jsont]

  let apply_edit edit t =
    match edit with
    | New_slug slug -> { t with slug }
    | New_name name -> { t with name }
    | New_description description -> { t with description }
    | New_places places -> { t with places }

  let store : t Dynarray.t = Dynarray.create ()

  let make ~slug ~name ?description ?places () =
    let places = Option.value ~default:[] places in
    let id = Dynarray.length store in
    let v : t = { id; slug; name; description; places } in
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
    name : string;
    friends : t list;
    availabilities : Availabilities.t;
  }
  [@@deriving jsont]

  type shallow = {
    id : int;
    name : string;
    friends : int list;
    availabilities : Availabilities.t;
  }

  type edit =
    | New_name of string
    | New_friends of t list
    | Update_availabilities of Availabilities.edit
  [@@deriving jsont]

  let apply_edit (edit : edit) (t : t) : t =
    match edit with
    | New_name name -> { t with name }
    | New_friends (friends : t list) -> { t with friends }
    | Update_availabilities edit ->
        {
          t with
          availabilities = Availabilities.apply_edit edit t.availabilities;
        }

  let store : t Dynarray.t = Dynarray.create ()

  let make ?friends ?availabilities ~name () =
    let friends = Option.value ~default:[] friends in
    let availabilities = Option.value ~default:CCRAL.empty availabilities in
    let id = Dynarray.length store in
    let v : t = { id; name; friends; availabilities } in
    Dynarray.add_last store v;
    v

  let to_shallow (t : t) : shallow =
    {
      id = t.id;
      name = t.name;
      friends = List.map ~f:(fun (t : t) -> t.id) t.friends;
      availabilities = t.availabilities;
    }

  let of_shallow (t : shallow) : t =
    {
      id = t.id;
      name = t.name;
      friends = List.map ~f:(Dynarray.get store) t.friends;
      availabilities = t.availabilities;
    }
end

module Volunteers = Random_access_list (Volunteer)

module Quest = struct
  type t = {
    id : int;
    slug : string;
    name : string;
    description : string option;
    task_type : Task_type.t;
    place : Place.t;
    start_minute : int;
    end_minute : int;
    required_volunteers : int;
  }
  [@@deriving jsont]

  type edit =
    | New_slug of string
    | New_name of string
    | New_description of string option
    | New_task_type of Task_type.t
    | New_place of Place.t
    | New_start_minute of int
    | New_end_minute of int
    | New_required_volunteers of int
  [@@deriving jsont]

  let apply_edit edit t =
    match edit with
    | New_slug slug -> { t with slug }
    | New_name name -> { t with name }
    | New_description description -> { t with description }
    | New_task_type task_type -> { t with task_type }
    | New_place place -> { t with place }
    | New_start_minute start_minute -> { t with start_minute }
    | New_end_minute end_minute -> { t with end_minute }
    | New_required_volunteers required_volunteers ->
        { t with required_volunteers }

  let store : t Dynarray.t = Dynarray.create ()

  let make ~slug ~name ?description ~task_type ~place ~start_minute ~end_minute
      ~required_volunteers () =
    let id = Dynarray.length store in
    let v : t =
      {
        id;
        slug;
        name;
        description;
        task_type;
        place;
        start_minute;
        end_minute;
        required_volunteers;
      }
    in
    Dynarray.add_last store v;
    v
end

module Quests = Random_access_list (Quest)

module Planning = struct
  type t = {
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
