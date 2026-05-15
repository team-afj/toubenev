open Ortools
open Lunar
open Data_repr
open Rich
open Normal

type t = {
  model : Sat.model;
  options : Options.t;
  with_assumptions : bool;
  assignations : Volunteer.t -> Quest.t -> Sat.Var.t_bool;
  intervals : Volunteer.t -> Quest.t -> Sat.interval_var;
  assignations_rev : int -> Sat.Var.t_bool * Volunteer.t * Quest.t;
  vs : Volunteers.t;
  qs : Quests.t;
  by_day : Quests.t Date.Map.t;
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
  let by_uuid : (Sat.Var.t_bool * Sat.interval_var) String.Map.t String.Map.t =
    Volunteers.fold vs ~init:String.Map.empty ~f:(fun acc (v : Volunteer.t) ->
        let quests =
          Quests.fold qs ~init:String.Map.empty ~f:(fun acc (q : Quest.t) ->
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
              String.Map.add q.id (var, interval) acc)
        in
        String.Map.add v.id quests acc)
  in
  let find =
   fun v q ->
    String.Map.find v.Volunteer.id by_uuid |> String.Map.find q.Quest.id
  in
  let find_assignation v q = fst (find v q) in
  let find_interval v q = snd (find v q) in
  let rev_find = fun i -> Hashtbl.find rev_tbl i in
  (find_assignation, find_interval, rev_find)

let prepare ~with_assumptions model (data : Planning.t) =
  (* TODO: split quests, group friends and quests groups, etc *)
  let { Api.volunteers = vs; quests = qs; diagnostics = _ } =
    Data_repr.Conv.normalize data
  in
  let task_types = CCRAL.to_list data.task_types |> Task_type.Set.of_list in
  let assignations, intervals, assignations_rev = assignations model vs qs in
  let for_all_quests f = Quests.iter qs ~f in
  let for_all_volunteers f = Volunteers.iter ~f vs in
  let by_day = quests_by_day data.infos qs in
  {
    model;
    with_assumptions;
    options = data.options;
    assignations;
    intervals;
    assignations_rev;
    vs;
    qs;
    by_day;
    task_types;
    for_all_quests;
    for_all_volunteers;
  }

let resolve_assignations (ctx : t) arr =
  Array.foldi arr ~init:Quest.Map.empty ~f:(fun acc i b ->
      if b = 0 then acc
      else
        (* There are more variables than assignations so it is expected that the
           last ones are missing. We could just loop on the number of
           assignations.
        *)
        match ctx.assignations_rev i with
        | exception Not_found -> acc
        | _name, v, q ->
            Quest.Map.update q
              (function
                | None -> Some (Volunteers.singleton v)
                | Some vs -> Some (Volunteers.add v vs))
              acc)

let prepare_answer date context (response : Ortools.Sat.Response.t) =
  let open Ortools.Sat.Response in
  let solution = resolve_assignations context response.solution in
  let solution =
    Quest.Map.to_list solution
    |> List.map ~f:(fun (quest, volunteers) ->
        let volunteers =
          Volunteers.to_list_map ~f:(fun v -> v.initial) volunteers
        in
        { Data_repr.Api.quest; volunteers })
  in
  let sufficient_assumptions_for_infeasibility =
    List.map
      ~f:(fun v -> Ortools.Sat.Var.to_string v)
      response.sufficient_assumptions_for_infeasibility
  in
  {
    Data_repr.Api.status = response.status;
    diagnostics = [];
    solution;
    sufficient_assumptions_for_infeasibility;
    log = response.solve_log;
    date;
  }
