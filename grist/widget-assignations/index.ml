open Brrer
open Brr
open Brr_lwd
open Lwd_infix
open! Fut.Result_syntax
open! Lunar_jsont
open! Data_repr
open! Rich
open Normal
open Tables

let () = Logs.set_reporter (Logs_browser.console_reporter ())
let () = Logs.set_level ~all:true (Some Debug)

module Logs = (val Logs.src_log (Logs.Src.create "TBNASS"))

(* This widget should be associated to the ASSIGNATION table and filtered by
   SOLUTION *)

module App_state = struct
  let active_assignations :
      (Jstr.t
      * Rich.Planning.t
      * Api.data
      * Api.assignation list
      * Shared.Analysis.t)
      option
      Lwd.var =
    Lwd.var None
end

let app =
  let render (name, _, data, assignations, analysis) =
    let title = Jstr.append (Jstr.v "Assignations de ") name in
    let per_volunteer = Infos.per_volunteer_el data assignations in
    let all_volunteers_sorted = Infos.list data assignations in
    let complete_diff_table = Diffs_table.make analysis in
    Elwd.div
      [
        `P (El.h2 [ El.txt title ]);
        `P (El.h3 [ El.txt' "Infos par bénévoles:" ]);
        `R per_volunteer;
        `P (El.hr ());
        `R
          (Pico_ui.accordion ~name:"all_v" ~closed:true
             ~title:
               (Lwd.return (El.txt' "Classement (des bénévoles insatisfaits)"))
             [ `P all_volunteers_sorted ]);
        `P (El.h3 [ El.txt' "Écarts de temps:" ]);
        `R complete_diff_table;
      ]
  in

  let content =
    let$* state = Lwd.get App_state.active_assignations in
    match state with
    | None -> Lwd.return (El.nbsp ())
    | Some state -> render state
  in
  Elwd.div [ `R content ]

let resolve_assignation_jv (data : Api.data) ass =
  let quest_id = Jv.Jstr.get ass "ref" |> Jstr.to_string in
  let volunteers_ids =
    let jv = Jv.get ass "volunteers" in
    if Jv.is_none jv then [] else Jv.to_list Jv.to_int jv
  in
  let quest = Quests.find_by_id quest_id data.quests in
  let by_id =
    let tbl = Hashtbl.create 128 in
    Volunteers.iter data.volunteers ~f:(fun v -> Hashtbl.add tbl v.id v);
    tbl
  in
  let volunteers =
    List.fold_left volunteers_ids ~init:Volunteers.empty ~f:(fun acc i ->
        try
          let v = Hashtbl.find by_id (string_of_int i) in
          Volunteers.add v acc
        with err ->
          Console.error [ "TBN ASS OUPS "; err ];
          acc)
  in
  { Api.quest; volunteers }

let on_records () =
  (* For custom widgets, add a handler that will be called whenever the selected
     records change. If the widget is correctly set this means rows from the
     ASSIGNATIONS table. *)
  let f =
   fun v ->
    match Jv.to_jv_list v with
    | [] -> Fut.ok ()
    | hd :: _ as assignations ->
        (* Solution should contain a ROW ID *)
        let solution_id = Jv.(Int.get (get hd "solution") "rowId") in
        let* solution = Solutions.get_solution solution_id in
        let name = Solutions.name solution in
        let* data = Solutions.data_rich solution |> Fut.return in
        let+ analyses = Solutions.analysis solution |> Fut.return in
        let normal = Conv.normalize data in
        (* Console.log
          [
            "TBN ASS DBG ALL V";
            Volunteers.fold normal.volunteers ~init:"" ~f:(fun s v ->
                s ^ " " ^ v.id ^ " " ^ v.name);
          ]; *)
        let assignations =
          List.map assignations ~f:(resolve_assignation_jv normal)
        in
        Lwd.set App_state.active_assignations
          (Some (name, data, normal, assignations, analyses))
  in
  let callback = Jv.callback ~arity:1 f in
  let options =
    Jv.obj [| ("keepEncoded", Jv.false'); ("expandRefs", Jv.false') |]
  in
  Grist.on_records ~callback ~options ()

let _ =
  let on_load _ =
    let () = on_records () in
    let root = El.find_first_by_selector (Jstr.v "main") |> Option.get in
    let app = Lwd.observe app in
    let f _ = ignore @@ Lwd.quick_sample app in
    let on_invalidate _ = ignore @@ G.request_animation_frame f in
    El.append_children root [ Lwd.quick_sample app ];
    Lwd.set_on_invalidate app on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)
