open Lunar_jsont

module type Editable = sig
  type t
  type edit

  val edit_jsont : edit Jsont.t
  val apply_edit : edit -> t -> t
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

type _ uuid

module Random_access_list : (X : S) -> sig
  include S with type t = X.t CCRAL.t
end

module Place : sig
  type t = private {
    id : t uuid;
    slug : string;
    name : string;
    description : string option;
  }

  include S with type t := t

  val make : slug:string -> name:string -> ?description:string -> unit -> t
end

module Places : sig
  include module type of Random_access_list (Place)
end

module Task_type : sig
  type t = private {
    id : t uuid;
    slug : string;
    name : string;
    description : string option;
    specialist_only : bool;
    divisible : bool;
  }

  include S with type t := t

  val make :
    slug:string ->
    name:string ->
    ?description:string ->
    specialist_only:bool ->
    divisible:bool ->
    unit ->
    t
end

module Task_types : sig
  include module type of Random_access_list (Task_type)
end

module Time_slot : sig
  type recurrence = Daily | Weekly of Weekday.t list | On of Date.t list
  [@@deriving jsont]

  type t = { recurrence : recurrence; start : Time.t; duration : Duration.t }
  [@@deriving jsont]

  include S with type t := t
end

module Time_slots : sig
  include module type of Random_access_list (Time_slot)
end

module Availability : sig
  type status =
    | Unavailable
    | Available of int
        (** The [int] argument quantifies the solution bonus or malus if this
            time slot is used *)
  [@@deriving jsont]

  type t = private { status : status; slot : Time_slot.t }

  include S with type t := t
end

module Availabilities : sig
  include module type of Random_access_list (Availability)
end

module Volunteer : sig
  type t = private {
    id : t uuid;
    public_name : string option;
    name : string;
    daily_workload : Duration.t;
    availabilities : Availabilities.t;
    arrival : Datetime.t option;
    departure : Datetime.t option;
    friends : t uuid list;
    ennemis : t uuid list;
    proficiencies : Task_types.t;
    forbidden_tasks : Task_types.t;
    forbidden_places : Places.t;
  }

  include S with type t := t

  val make :
    ?friends:t uuid list ->
    ?ennemis:t uuid list ->
    ?proficiencies:Task_types.t ->
    ?forbidden_tasks:Task_types.t ->
    ?forbidden_places:Places.t ->
    ?availabilities:Availabilities.t ->
    ?arrival:Datetime.t ->
    ?departure:Datetime.t ->
    daily_workload:Duration.t ->
    name:string ->
    ?public_name:string ->
    unit ->
    t
end

module Volunteers : sig
  include module type of Random_access_list (Volunteer)
end

module Quest : sig
  type t = private {
    id : t uuid;
    name : string;
    description : string option;
    task_type : Task_type.t;
    place : Place.t;
    slot : Time_slot.t;
    required_volunteers : int;
  }

  include S with type t := t

  val make :
    name:string ->
    ?description:string ->
    task_type:Task_type.t ->
    place:Place.t ->
    slot:Time_slot.t ->
    required_volunteers:int ->
    unit ->
    t
end

module Quests : sig
  include module type of Random_access_list (Quest)
end

module Event_infos : sig
  type kind = Finite of { start_date : Date.t; end_date : Date.t }
  [@@deriving jsont]

  type t = { name : string; kind : kind } [@@deriving jsont]
end

module Planning : sig
  type t = {
    info : Event_infos.t;
    places : Places.t;
    task_types : Task_types.t;
    volunteers : Volunteers.t;
    quests : Quests.t;
  }

  type edit =
    | Places of Places.edit
    | Task_types of Task_types.edit
    | Volunteers of Volunteers.edit
    | Quests of Quests.edit

  include S with type t := t and type edit := edit
end
