open Std
open! Lunar
open Types
open Ortools
module RAL = CCRAL

type context = {
  model : Sat.model;
  assignations : Volunteer.t -> Quest.t -> [ `Bool ] Sat.Var.t;
  vs : Volunteer.t list;
  qs : Quest.t list;
  for_all_quests : (Quest.t -> unit) -> unit;
  for_all_volunteers : (Volunteer.t -> unit) -> unit;
}

let assignations m vs qs =
  let c = ref 0 in
  let tbl : (Volunteer.t uuid * Quest.t uuid, Sat.Var.t_bool) Hashtbl.t =
    let tbl = Hashtbl.create (List.length vs * List.length qs) in
    List.iter vs ~f:(fun (v : Volunteer.t) ->
        List.iter qs ~f:(fun (q : Quest.t) ->
            incr c;
            Format.sprintf "%i_%s_is_assigned_to_%s" !c v.name q.name
            |> Sat.Var.new_bool m
            |> Hashtbl.add tbl (v.id, q.id)));
    tbl
  in
  fun v q -> Hashtbl.find tbl (v.Volunteer.id, q.Quest.id)

let prepare model (data : Planning.t) =
  (* TODO: split quests, group friends and quests groups, etc *)
  let vs = RAL.to_list data.volunteers in
  let qs = RAL.to_list data.quests in
  let assignations = assignations model vs qs in
  let for_all_quests f = List.iter qs ~f in
  let for_all_volunteers f = List.iter vs ~f in
  { model; assignations; vs; qs; for_all_quests; for_all_volunteers }

(** All quests are fully staffed *)
let all_staffed (ctx : context) =
  let quest_is_staffed (q : Quest.t) =
    let open Sat in
    let name = Format.sprintf "q_%s_is_staffed" q.name in
    let sum =
      LinearExpr.sum_vars @@ List.map ctx.vs ~f:(fun v -> ctx.assignations v q)
    in
    sum == of_int q.required_volunteers |> add ctx.model ~name
  in
  ctx.for_all_quests quest_is_staffed

let make (data : Planning.t) =
  let model = Sat.make ~name:"Toubenev" () in
  let context = prepare model data in

  let () = all_staffed context in

  model
