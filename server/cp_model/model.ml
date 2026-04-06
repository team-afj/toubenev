open Std
open! Lunar
open Types
open Ortools
module RAL = CCRAL
module Uuidm_map = Map.Make (Uuidm)
open Norm

type context = {
  model : Sat.model;
  options : Options.t;
  assignations : Volunteer.t -> Quest.t -> Sat.Var.t_bool;
  assignations_rev : int -> Sat.Var.t_bool * Volunteer.t * Quest.t;
  vs : Volunteers.t;
  qs : Quests.t;
  for_all_quests : (Quest.t -> unit) -> unit;
  for_all_volunteers : (Volunteer.t -> unit) -> unit;
}

let assignations m vs qs =
  let size = Volunteers.cardinal vs * Quests.cardinal qs in
  let c = ref 0 in
  let rev_tbl : (int, Sat.Var.t_bool * Volunteer.t * Quest.t) Hashtbl.t =
    Hashtbl.create size
  in
  let by_uuid =
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
              Uuidm_map.add q.id var acc)
        in
        Uuidm_map.add v.id quests acc)
  in
  let find =
   fun v q -> Uuidm_map.find v.Volunteer.id by_uuid |> Uuidm_map.find q.Quest.id
  in
  let rev_find = fun i -> Hashtbl.find rev_tbl i in
  (find, rev_find)

let prepare model (data : Planning.t) =
  (* TODO: split quests, group friends and quests groups, etc *)
  let vs =
    RAL.to_list data.volunteers
    |> List.map ~f:Volunteer.normalize
    |> Volunteers.of_list
  in
  let qs =
    RAL.to_list data.quests
    |> List.concat_map ~f:(Quest.normalize data.info vs)
    |> Quests.of_list
  in
  let assignations, assignations_rev = assignations model vs qs in
  let for_all_quests f = Quests.iter qs ~f in
  let for_all_volunteers f = Volunteers.iter ~f vs in
  {
    model;
    options = data.options;
    assignations;
    assignations_rev;
    vs;
    qs;
    for_all_quests;
    for_all_volunteers;
  }

(** Utilities *)
let is_false v = Sat.(var v == of_int 0)

let is_true v = Sat.(var v == of_int 1)

(** All quests are fully staffed *)
let all_staffed (ctx : context) =
  let quest_is_staffed (q : Quest.t) =
    let open Sat in
    let name = Format.sprintf "q_%s_is_staffed" q.name in
    let sum =
      LinearExpr.sum_vars
      @@ Volunteers.to_list_map ctx.vs ~f:(fun v -> ctx.assignations v q)
    in
    sum == of_int q.initial.required_volunteers |> add ctx.model ~name
  in
  ctx.for_all_quests quest_is_staffed

(** Volunteers cannot do several things at the same time *)
let non_ubiquity_of_normal_humans (ctx : context) =
  ctx.for_all_quests @@ fun q ->
  let overlapping = Quest.overlaps_with ctx.options q ctx.qs in
  Quests.iter overlapping ~f:(fun q' ->
      if not (Quest.equal q q') then
        ctx.for_all_volunteers @@ fun v ->
        let assig_v = ctx.assignations v in
        let name =
          Format.sprintf "%s_cannot_do_both_%s_and_%s" v.initial.name q.name
            q'.name
        in
        Sat.Constraint.at_most_one [ assig_v q; assig_v q' ]
        |> Sat.add ctx.model ~name)

(** Enforces manual assignations of volunteers, and prevents manually assigned
    volunteers from doing anything else. *)
let enforce_assignations (ctx : context) =
  let assigned = Hashtbl.create 16 in
  let () =
    ctx.for_all_quests @@ fun q ->
    Volunteers.iter q.assigned_volunteers ~f:(fun (v : Volunteer.t) ->
        let name = Format.sprintf "%s_assigned_to_%s" v.initial.name q.name in
        Hashtbl.add assigned (q.id, v.id) ();
        Sat.(add ctx.model ~name (is_true (ctx.assignations v q))))
  in
  ctx.for_all_volunteers @@ fun v ->
  if v.initial.manually_assigned then
    ctx.for_all_quests @@ fun q ->
    if not (Hashtbl.mem assigned (q.id, v.id)) then
      let name =
        Format.sprintf "manually_assigned_%s_cannot_do_%s" v.initial.name q.name
      in
      Sat.(add ctx.model ~name (is_false (ctx.assignations v q)))

let make (data : Planning.t) =
  let model = Sat.make ~name:"Toubenev" () in
  let context = prepare model data in

  let () = all_staffed context in
  let () = non_ubiquity_of_normal_humans context in
  let () = enforce_assignations context in

  context

let resolve_solution ctx arr =
  Array.mapi arr ~f:(fun i b ->
      let name, v, q = ctx.assignations_rev i in
      (name, v, q, b))

let pp_solution fmt arr =
  let open Format in
  let pp_var fmt (var, _v, _q, b) =
    fprintf fmt "%s: %b" (Sat.Var.to_string var) (Bool.of_int b)
  in
  let pp_sep fmt () = fprintf fmt ";@ " in
  fprintf fmt "%a" (pp_print_array ~pp_sep pp_var) arr
