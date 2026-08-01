open Data_repr
open Rich
open Normal
open Lunar_jsont

val v_is_manually_assigned_to_q : Volunteer.t -> Quest.t -> bool

type with_cache = {
  can_do_res : Volunteers.elt -> Quests.elt -> (unit, string) result;
  can_do : Volunteers.elt -> Quests.elt -> bool;
  max_doable : ?key:string -> Volunteers.elt -> Quests.t -> Duration.t;
}

val make : Event_infos.t -> Quests.t -> unit -> with_cache
