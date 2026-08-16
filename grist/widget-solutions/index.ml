open Brrer
open Brr
open Brr_lwd
open! Lwd_infix
open! Fut.Result_syntax
open! Lunar_jsont
open! Data_repr
open! Rich
open! Normal
open! Tables

let () = Logs.set_reporter (Logs_browser.console_reporter ())
let () = Logs.set_level ~all:true (Some Debug)

module Logs = (val Logs.src_log (Logs.Src.create "TBNSOL"))

(* This widget should be associated to the ASSIGNATION table and filtered by
   SOLUTION *)

let grist_set_pointer id =
  let pos = Jv.obj [| ("rowId", Jv.of_int (int_of_string id)) |] in
  (* This requires ready({allowSelectBy:true}) *)
  Grist.set_cursor_pos ~pos

let get_solution id : (int * string * Jstr.t) option =
  Lwd_seq.to_list (Lwd.peek App_state.solutions)
  |> List.find_opt ~f:(fun (i, _, _) -> id = i)

let now () =
  Unix.time () |> Float.to_int |> Duration.from_seconds
  |> Zoned_datetime.from_duration
  |> Zoned_datetime.change_timezone ~tz:(Timezone.make ~hour:2 ~min:0)
  |> Zoned_datetime.to_local_datetime

let copy_from id =
  let id = int_of_string id in
  let _, _name, data = get_solution id |> Option.get_exn_or "bad solution" in
  let* assignations = Tables.Assignations.get_assignations ~solution:id in
  let assignations =
    List.map assignations ~f:(fun obj ->
        ( Jv.get obj "initial_quest",
          Jv.get obj "ref",
          Jv.get obj "start",
          Jv.get obj "end_",
          Jv.get obj "volunteers",
          Jv.get obj "assigned_volunteers",
          Jv.get obj "keep" ))
  in
  let now = now () |> Datetime.to_string in
  let+ res = Solutions.create_solution ~name:(Jstr.v now) data in
  let solution = Jv.Int.get (Jv.Jarray.get res 0) "id" in
  Tables.Assignations.create_solution_assignations ~solution assignations

let delete ~solution =
  let* _ = Tables.Assignations.remove_assignations ~solution in
  Tables.Solutions.remove ~solution

let solution_placeholder (quests : Normal.Quests.t) =
  Normal.Quests.to_list_map
    ~f:(fun quest -> { Api.quest; volunteers = quest.assigned_volunteers })
    quests

let unwind_planning () =
  Fut.bind (Data.fetch_all ()) @@ function
  | Error jv_err ->
      Console.error [ "DBG"; "Decoding error: "; jv_err ];
      let err =
        Jv.Error.message jv_err |> Jstr.to_string
        |> String.replace ~sub:"[0m" ~by:"\""
        |> String.replace ~sub:"[1m" ~by:"\""
      in
      let error =
        [
          El.p
            [
              El.txt'
                "Une erreur est survenue lors de la lecture des données. Cela \
                 signifie probablement que le type d'une colonne n'est pas \
                 respéctée ou qu'une information nécéssaire est manquante.";
            ];
          El.pre [ El.txt' err ];
        ]
      in
      Pico_ui.Modal.one_shot ~title:"Oups !" error;
      Fut.error jv_err
  | Ok grist_data ->
      let _id_map, planning = Grist_import.to_planning grist_data in
      let normalized_planning = Conv.normalize planning in
      let initial_answer =
        {
          Api.dummy_answer with
          solution = solution_placeholder normalized_planning.quests;
          diagnostics = normalized_planning.diagnostics;
        }
      in
      let analysis =
        Shared.Analysis.of_planning planning initial_answer.solution
          normalized_planning
      in
      let state =
        {
          Tables.Solutions.data_rich = planning;
          answer = initial_answer;
          analysis;
        }
      in
      Fut.ok (state, grist_data, normalized_planning)

let server_sat_check_query data =
  let open Brr_io.Fetch in
  let* data = Fut.return @@ Jsont_brr.encode Grist_import.data_jsont data in
  let body = Body.of_jstr data in
  Console.debug [ "TBN"; "Querying server" ];
  let method' = Jstr.v "PUT" in
  let uri = Jstr.v "https://localhost:5173/check-data" in
  let headers =
    Headers.of_assoc [ (Jstr.v "Content-Type", Jstr.v "application/json") ]
  in
  let init = Request.init ~body ~method' ~headers () in
  let open Fut.Syntax in
  let () = Lwd.set App_state.server_status Working in
  let* response = url ~init uri in
  match response with
  | Error err ->
      Console.info [ "TBN"; "Error querying server" ];
      let* _ = Titles.update_prefixes "⛓️‍💥" in
      let () = Lwd.set App_state.server_status Offline in
      Fut.error err
  | Ok resp ->
      Console.info [ "TBN"; "Got server response" ];
      let () = Lwd.set App_state.server_status Done in
      let open Fut.Result_syntax in
      let* body = Response.as_body resp |> Body.json in
      Fut.return @@ Jsont_brr.decode_jv Data_repr.Api.answer_jsont body

let rev_append_diags diags (answer : Api.answer) =
  { answer with diagnostics = List.rev_append diags answer.diagnostics }

let server_sat_check (state : Solutions.t) grist_data normalized_planning =
  let open Fut.Syntax in
  let* response = server_sat_check_query grist_data in
  let initial_answer = state.answer in
  let planning = state.data_rich in
  let open Fut.Result_syntax in
  match response with
  | Error jv ->
      let answer =
        rev_append_diags
          [ (Error, Jv.Error.message jv |> Jstr.to_string) ]
          initial_answer
      in
      let state = { state with answer } in
      (* Solutions.upsert_solution_1 state *)
      Fut.ok state
  | Ok answer ->
      let* answer =
        match answer.status with
        | Feasible | Optimal ->
            let+ () = Titles.update_prefixes "🟢" in
            answer
        | Unknown | ModelInvalid | Infeasible ->
            let+ () = Titles.update_prefixes "🔴" in
            { answer with solution = initial_answer.solution }
      in
      let answer = rev_append_diags initial_answer.diagnostics answer in
      Console.debug [ "TBN"; "Perform analysis on assignations." ];
      let analysis =
        Shared.Analysis.of_planning planning answer.solution normalized_planning
      in
      let state = { state with answer; analysis } in
      let () =
        match answer.status with
        | Feasible | Optimal -> Lwd.set App_state.optimize_state (Ready state)
        | Api.Unknown | Api.ModelInvalid | Api.Infeasible ->
            Lwd.set App_state.optimize_state Not_ready
      in
      Fut.ok state
(* Solutions.upsert_solution_1 state *)
(* Fut.ok @@ Lwd.set App_state.last_answer (Some state) *)

let update_solution state =
  let s_id_1 = 1 in
  let* () = Assignations.remove_assignations ~solution:s_id_1 in
  let* () = Solutions.upsert_solution s_id_1 state in
  let assignations =
    List.map state.answer.solution
      ~f:(Grist_import.Assignation.v ~solution:s_id_1)
  in
  Assignations.insert_assignations assignations

let app =
  let controls =
    let in_progress = Lwd.var false in
    let check_btn =
      let disabled =
        Lwd.map (Lwd.get in_progress) ~f:(function
          | true -> At.disabled
          | false -> At.void)
      in
      let ev =
        Elwd.handler Ev.click (fun _ ->
            Lwd.set in_progress true;
            let fut =
              let* new_state, grist_data, normalized_planning =
                unwind_planning ()
              in
              let* new_state =
                server_sat_check new_state grist_data normalized_planning
              in
              update_solution new_state
            in
            Fut.await fut (fun _ -> Lwd.set in_progress false))
      in
      Elwd.button
        ~at:[ `R disabled ]
        ~ev:[ `P ev ]
        [
          `P (El.txt' "1. Déplier les quêtes et vérifier la faisabilité");
          `P (El.br ());
          `P
            (El.span
               ~at:[ At.style (Jstr.v "font-size:0.5rem") ]
               [ El.txt' "(remplace la solution \"Aperçu\")" ]);
        ]
    in
    let btns =
      Elwd.fieldset
        ~at:[ `P (At.v (Jstr.v "role") (Jstr.v "group")) ]
        [ `R check_btn ]
    in
    Pico_ui.Elwd.section [ `R btns ]
  in
  let solution_manager =
    let focus_btn =
      let disabled =
        Lwd.map2 (Lwd.get App_state.selected_in_grist)
          App_state.selected_solution ~f:(fun g f ->
            if g = int_of_string f then At.disabled else At.void)
      in
      let handler =
        Elwd.handler Ev.click (fun _ ->
            ignore @@ grist_set_pointer (Lwd.peek App_state.sol_select.value))
      in
      Pico_ui.Elwd.button
        ~at:[ `R disabled ]
        ~ev:[ `P handler ]
        [ `P (El.txt' "Selectioner") ]
    in
    let copy_btn =
      let in_progress = Lwd.var false in
      let disabled =
        Lwd.map (Lwd.get in_progress) ~f:(function
          | true -> At.disabled
          | false -> At.void)
      in
      Pico_ui.Elwd.button
        ~at:[ `R disabled ]
        ~ev:
          [
            `P
              (Elwd.handler Ev.click (fun _ ->
                   Lwd.set in_progress true;
                   Fut.await (copy_from (Lwd.peek App_state.sol_select.value))
                   @@ fun _ -> Lwd.set in_progress false));
          ]
        [ `P (El.txt' "Copy") ]
    in
    let delete_btn =
      let in_progress = Lwd.var false in
      let disabled =
        Lwd.map2 App_state.selected_solution (Lwd.get in_progress)
          ~f:(fun i p ->
            if p || int_of_string i <= 2 then At.disabled else At.void)
      in
      Pico_ui.Elwd.button
        ~at:[ `R disabled ]
        ~ev:
          [
            `P
              (Elwd.handler Ev.click (fun _ ->
                   Lwd.set in_progress true;
                   let solution =
                     int_of_string (Lwd.peek App_state.sol_select.value)
                   in
                   Fut.await (delete ~solution) @@ fun _ ->
                   Lwd.set in_progress false));
          ]
        [ `P (El.txt' "Delete") ]
    in
    let print_btn =
      let$* print_options_modal, show_modal =
        Lwd.map App_state.active_solution_normal ~f:(fun sol ->
            Print.modal (fun () ->
                match sol with
                | None -> Fut.ok None
                | Some (sol : Solutions.normal) ->
                    let solution =
                      Lwd.peek App_state.sol_select.value |> int_of_string
                    in
                    let+ assignations =
                      Assignations.get_assignations ~solution
                    in
                    let assignations =
                      List.map assignations
                        ~f:(Assignations.resolve_assignation_jv sol.data)
                    in
                    Some (sol.state.data_rich, assignations)))
      in
      let disabled =
        let$ answer = App_state.active_solution_state in
        match answer with
        | None | Some { answer = { solution = []; _ }; _ } -> At.disabled
        | Some _ -> At.void
      in
      let ev =
        let f = fun _ -> Lwd.set show_modal true in
        Elwd.handler Ev.click f
      in
      Elwd.span
        [
          `R print_options_modal;
          `R
            (Elwd.fieldset
               ~at:[ `P (At.v (Jstr.v "role") (Jstr.v "group")) ]
               [
                 `R
                   (Elwd.button
                      ~at:[ `R disabled ]
                      ~ev:[ `P ev ]
                      [ `P (El.txt' "Imprimer") ]);
               ]);
        ]
    in
    let actions =
      Elwd.fieldset
        ~at:[ `P (At.v (Jstr.v "role") (Jstr.v "group")) ]
        [ `R focus_btn; `R copy_btn; `R delete_btn ]
    in
    Pico_ui.Elwd.section
      [
        `P (El.h3 [ El.txt' "Gestion des solutions" ]);
        `R App_state.sol_select.field;
        `R actions;
        `R print_btn;
      ]
  in
  (* let dbg_planning =
    let$ answer = App_state.active_solution_state in
    match answer with
    | None | Some { answer = { solution = []; _ }; _ } -> El.nbsp ()
    | Some sol ->
        Render.make_plannings sol.data_rich sol.answer ~details:false
          [ `List_tasks_by_volunteer ]
  in *)
  let analyses =
    let$* results = App_state.active_solution_normal in
    match results with
    | None ->
        Lwd.return
          (El.h3
             [ El.txt' "Cette solution ne contient pas de données dépliées." ])
    | Some n ->
        let status =
          Printf.sprintf "%s after %im on %s"
            (n.state.answer.status |> Ortools.Sat.Response.string_of_status)
            (Int.of_float (n.state.answer.user_time /. 60.))
            (n.state.answer.date |> Zoned_datetime.to_local_datetime
           |> Datetime.to_string)
        in
        let capacity_table = Infos.capacity_table n in
        let available_volunteers =
          Infos.available_volunteers_widget n.state.data_rich
        in
        Pico_ui.Elwd.section
          [
            `P (El.h3 [ El.txt' "Analyses des données initiales" ]);
            `P (El.span [ El.txt' "Status: "; El.txt' status ]);
            `P (El.h4 [ El.txt' "Compteur de bénévoles" ]);
            `R available_volunteers;
            `P (El.h4 [ El.txt' "Main d'oeuvre requise / disponible" ]);
            `R capacity_table;
          ]
  in
  (* let dbg_grid_plan =
    let$ answer = App_state.active_solution_state in
    match answer with
    | None | Some { answer = { solution = []; _ }; _ } -> El.nbsp ()
    | Some sol ->
        El.div @@ Grid_planning.render sol.data_rich.infos sol.answer.solution
  in *)
  Elwd.div [ `R controls; `R solution_manager; `R analyses ]

let decode_solution_jv sol =
  let sol_id = Jv.Int.get sol "id" in
  let sol_name = Jv.Jstr.get sol "name" |> Jstr.to_string in
  let sol_raw = Jv.Jstr.get sol "last_answer" in
  (sol_id, sol_name, sol_raw)

let on_record () =
  let f =
   fun v ->
    let id = Jv.Int.get v "id" in
    Lwd.set App_state.sol_select.value (string_of_int id);
    Lwd.set App_state.selected_in_grist id
  in
  let callback = Jv.callback ~arity:1 f in
  let options =
    Jv.obj [| ("keepEncoded", Jv.false'); ("expandRefs", Jv.false') |]
  in
  Grist.on_record ~callback ~options ()

let fetch_solutions () =
  let sols = Lwd.peek App_state.solutions in
  if Equal.poly (Lwd_seq.view sols) Lwd_seq.Empty then
    ignore
    @@
    let+ sols = Tables.Data.(fetch solutions_tbl_id) in
    let sols = Jv.to_list decode_solution_jv sols in
    Lwd.set App_state.solutions (Lwd_seq.of_list sols)

let on_records () =
  let f =
   fun v ->
    let sols = Jv.to_list decode_solution_jv v in
    (* TODO update active solution *)
    (* let current = Lwd.peek App_state.sol_select.value in *)
    Lwd.set App_state.solutions (Lwd_seq.of_list sols)
  in
  let callback = Jv.callback ~arity:1 f in
  let options =
    Jv.obj [| ("keepEncoded", Jv.false'); ("expandRefs", Jv.false') |]
  in
  Grist.on_records ~callback ~options ()

let _ =
  let on_load _ =
    let () = on_record () in
    let () = on_records () in
    let _ =
      (* This is jank to fix jank: there are some race conditions between on_record and on_records *)
      G.set_timeout ~ms:2000 fetch_solutions
    in
    let root = El.find_first_by_selector (Jstr.v "main") |> Option.get in
    let app = Lwd.observe app in
    let f _ = ignore @@ Lwd.quick_sample app in
    let on_invalidate _ = ignore @@ G.request_animation_frame f in
    El.append_children root [ Lwd.quick_sample app ];
    Lwd.set_on_invalidate app on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)
