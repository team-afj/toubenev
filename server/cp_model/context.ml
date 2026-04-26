open Ortools
open Datarepr.Rich
open Datarepr.Normal
module Uuidm_map = Map.Make (Uuidm)

type t = {
  model : Sat.model;
  options : Options.t;
  with_assumptions : bool;
  assignations : Volunteer.t -> Quest.t -> Sat.Var.t_bool;
  intervals : Volunteer.t -> Quest.t -> Sat.interval_var;
  assignations_rev : int -> Sat.Var.t_bool * Volunteer.t * Quest.t;
  vs : Volunteers.t;
  qs : Quests.t;
  task_types : Task_type.Set.t;
  for_all_quests : (Quest.t -> unit) -> unit;
  for_all_volunteers : (Volunteer.t -> unit) -> unit;
}

let assignations m vs qs =
  let size = Volunteers.cardinal vs * Quests.cardinal qs in
  let c = ref 0 in
  let rev_tbl : (int, Sat.Var.t_bool * Volunteer.t * Quest.t) Hashtbl.t =
    Hashtbl.create size
  in
  let by_uuid : (Sat.Var.t_bool * Sat.interval_var) Uuidm_map.t Uuidm_map.t =
    Volunteers.fold vs ~init:Uuidm_map.empty ~f:(fun acc (v : Volunteer.t) ->
        let quests =
          Quests.fold qs ~init:Uuidm_map.empty ~f:(fun acc (q : Quest.t) ->
              let name =
                Format.sprintf "%i_%s_is_assigned_to_%s" !c v.initial.name
                  q.name
              in
              let var = Sat.Var.new_bool m name in
              Hashtbl.add rev_tbl !c (var, v, q);
              incr c;
              let interval =
                let open Lunar in
                let datetime_to_minutes d =
                  Datetime.to_duration d |> Duration.to_minutes
                  |> Sat.LinearExpr.of_int
                in
                let start = datetime_to_minutes q.slot.start in
                let end_ = datetime_to_minutes (Time_slot.end_ q.slot) in
                let size = Sat.(end_ - start) in
                let name =
                  Format.sprintf "%i_interval_%s_does_%s" !c v.initial.name
                    q.name
                in
                Sat.new_optional_interval_var m ~start ~size ~end_
                  ~is_present:var name
              in
              Uuidm_map.add q.id (var, interval) acc)
        in
        Uuidm_map.add v.id quests acc)
  in
  let find =
   fun v q -> Uuidm_map.find v.Volunteer.id by_uuid |> Uuidm_map.find q.Quest.id
  in
  let find_assignation v q = fst (find v q) in
  let find_interval v q = snd (find v q) in
  let rev_find = fun i -> Hashtbl.find rev_tbl i in
  (find_assignation, find_interval, rev_find)

let prepare ~with_assumptions model (data : Planning.t) =
  (* TODO: split quests, group friends and quests groups, etc *)
  let vs =
    CCRAL.to_list data.volunteers
    |> List.map ~f:(Volunteer.normalize data.infos)
    |> Volunteers.of_list
  in
  let qs =
    CCRAL.to_list data.quests
    |> List.concat_map ~f:(Quest.normalize data.infos vs)
    |> Quests.of_list
  in
  let task_types = CCRAL.to_list data.task_types |> Task_type.Set.of_list in
  let assignations, intervals, assignations_rev = assignations model vs qs in
  let for_all_quests f = Quests.iter qs ~f in
  let for_all_volunteers f = Volunteers.iter ~f vs in
  {
    model;
    with_assumptions;
    options = data.options;
    assignations;
    intervals;
    assignations_rev;
    vs;
    qs;
    task_types;
    for_all_quests;
    for_all_volunteers;
  }
