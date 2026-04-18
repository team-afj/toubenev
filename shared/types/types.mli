open Lunar_jsont
module Timezones = Timezones

(** Utilities *)
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

val uuid_equal : 'a uuid -> 'a uuid -> bool
val uuid_to_uuidm : 'a uuid -> Uuidm.t

module Random_access_list : (X : S) -> sig
  include S with type t = X.t CCRAL.t
end

(** Model *)

module Event_infos : sig
  type kind = Finite of { start_date : Date.t; end_date : Date.t }
  [@@deriving jsont]

  type t = { name : string; kind : kind; timezone : Timezone.t }
  [@@deriving jsont]
end

module Options : sig
  type t = {
    minimum_transfer_time : Duration.t;
        (** The free time volunteers should have to move between two quests that
            are in different places. *)
  }
  [@@deriving jsont]

  val default : t
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
  (* Only [At_least_once] is handled right now *)
  type task_sharing = Not_necessarily | At_least_once | In_equal_proportion

  type t = private {
    id : t uuid;
    slug : string;
    name : string;
    description : string option;
    everyone_should_do_it : task_sharing;
    specialist_only : bool;
    divisible : bool;
  }

  val equal : t -> t -> bool

  module Set : Set.S with type elt = t
  include S with type t := t

  val make :
    slug:string ->
    name:string ->
    ?description:string ->
    ?everyone_should_do_it:task_sharing ->
    specialist_only:bool ->
    divisible:bool ->
    unit ->
    t
end

module Task_types : sig
  include module type of Random_access_list (Task_type)
end

module Time_spec : sig
  type recurrence = Daily | Weekly of Weekday.Set.t | On of Date.t list
  [@@deriving jsont]

  type t = {
    recurrence : recurrence;
    start : Time.t;
    duration : Duration.t;
    first_day : Date.t option;
    last_day : Date.t option;
  }
  [@@deriving jsont]

  include S with type t := t

  val make :
    recurrence ->
    ?first_day:Date.t ->
    ?last_day:Date.t ->
    Time.t ->
    Duration.t ->
    t
end

module Time_specs : sig
  include module type of Random_access_list (Time_spec)
end

module Availability : sig
  type status =
    | Unavailable
    | Available of int
        (** The [int] argument quantifies the solution bonus or malus if this
            time slot is used *)
  [@@deriving jsont]

  type t = { status : status; slot : Time_spec.t }

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
    manually_assigned : bool;
    availabilities : Availabilities.t;
    arrival : Datetime.t option;
    departure : Datetime.t option;
    mutable friends : t uuid list;
    mutable ennemis : t uuid list;
    proficiencies : Task_types.t;
    forbidden_tasks : Task_types.t;
    forbidden_places : Places.t;
  }

  include S with type t := t

  val dummy : t

  val make :
    ?friends:t uuid list ->
    ?ennemis:t uuid list ->
    ?proficiencies:Task_types.t ->
    ?forbidden_tasks:Task_types.t ->
    ?forbidden_places:Places.t ->
    ?availabilities:Availabilities.t ->
    ?arrival:Datetime.t ->
    ?departure:Datetime.t ->
    ?manually_assigned:bool ->
    daily_workload:Duration.t ->
    name:string ->
    ?public_name:string ->
    unit ->
    t

  val set_friends : t -> t uuid list -> unit
  val set_ennemis : t -> t uuid list -> unit
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
    slot : Time_spec.t;
    required_volunteers : int;
    assigned_volunteers : Volunteers.t;
  }

  val dummy : t

  include S with type t := t

  val make :
    name:string ->
    ?description:string ->
    task_type:Task_type.t ->
    place:Place.t ->
    slot:Time_spec.t ->
    required_volunteers:int ->
    ?assigned_volunteers:Volunteers.t ->
    unit ->
    t
end

module Quests : sig
  include module type of Random_access_list (Quest)
end

module Planning : sig
  type t = {
    options : Options.t;
    infos : Event_infos.t;
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
