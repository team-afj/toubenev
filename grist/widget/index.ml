open Brrer
open Brr
open Brr_lwd
open Fut.Result_syntax
open Lunar_jsont
open! Data_repr
open Tables

let () = Logs.set_reporter (Logs_browser.console_reporter ())
let () = Logs.set_level ~all:true (Some Debug)

module Logs = (val Logs.src_log (Logs.Src.create "TBN"))

(* TODO: There might be more efficient way to do some tthings by using the REST
API with short-live tokens. *)

(* Current (26/05/2026) dataflow:
  -- Fetch from Grist widget API -> [Grist_import.data]
     -- [Sent to the server fot sat check ] -> [Api.answer]
  -- [Grist_import.to_planning] -> [Rich.Planning.t]
  -- [Conv.normalize] -> [Api.data]
     -- Used for "Dummy assignations" -> [Grist_import.Assignation.t list]
*)

let solution_placeholder (quests : Normal.Quests.t) =
  Normal.Quests.to_list_map
    ~f:(fun quest -> { Api.quest; volunteers = quest.assigned_volunteers })
    quests

let rev_append_diags diags (answer : Api.answer) =
  { answer with diagnostics = List.rev_append diags answer.diagnostics }

let sat =
  let s_id_1 = 1 in
  let last_data = ref None in
  fun () ->
    Fut.bind (Data.fetch_all ()) @@ fun data ->
    match (data, !last_data) with
    | Error err, _ ->
        Console.error [ "DBG"; "Decoding error: "; Jv.Error.message err ];
        let err =
          Jv.Error.message err |> Jstr.to_string
          |> String.replace ~sub:"[0m" ~by:"\""
          |> String.replace ~sub:"[1m" ~by:"\""
        in
        let error =
          [
            El.p
              [
                El.txt'
                  "Une erreur est survenue lors de la lecture des données. \
                   Cela signifie probablement que le type d'une colonne n'est \
                   pas respéctée ou qu'une information nécéssaire est \
                   manquante.";
              ];
            El.pre [ El.txt' err ];
          ]
        in
        Pico_ui.Modal.one_shot ~title:"Oups !" error;
        Fut.return (Ok ())
    | Ok data, Some last when Equal.poly last data ->
        let () = Console.info [ "TBN"; "Nothing to do, data didn't change" ] in
        let last = Lwd.peek App_state.last_answer in
        let grist_data = data in
        Option.iter
          (fun (data : Solutions.t) ->
            if Equal.poly data.answer.status Feasible then
              Lwd.set App_state.optimize_state (Ready (grist_data, data)))
          last;
        Fut.return (Ok ())
    | Ok data, _ -> begin
        let () = Lwd.set App_state.optimize_state Not_ready in
        let () = Console.info [ "TBN"; "Data changed" ] in
        let () = last_data := Some data in
        let grist_data = data in
        let _id_map, planning = Grist_import.to_planning data in
        let () = Console.debug [ "TBN"; "Normalize" ] in
        let normalized_planning = Conv.normalize planning in
        let () =
          let groups =
            List.map ~f:(fun (id, g) ->
                Jstr.v @@ "[" ^ id ^ "] " ^ Normal.Quests_group.to_string g)
            @@ String.Map.to_list normalized_planning.quests_groups
          in
          Console.debug [ "TBN"; "Groups"; Jv.of_jstr_list groups ]
        in
        let initial_answer =
          {
            Api.dummy_answer with
            solution = solution_placeholder normalized_planning.quests;
            diagnostics = normalized_planning.diagnostics;
          }
        in
        (* New assignations (unfolded quests) *)
        let () = Console.debug [ "TBN"; "Prepare empty assignations" ] in
        let assignations =
          let open Normal in
          Quests.to_list_map
            ~f:(fun { Quest.id; name; initial; slot; _ } ->
              let initial_quest = Rich.id_to_int initial.id in
              let start =
                Zoned_datetime.to_utc_duration slot.start |> Duration.to_seconds
              in
              let end_ =
                Zoned_datetime.(slot.start + slot.duration)
                |> Zoned_datetime.to_utc_duration |> Duration.to_seconds
              in
              {
                Grist_import.Assignation.solution = s_id_1;
                name;
                ref = id;
                initial_quest;
                start;
                end_;
                volunteers =
                  CCRAL.to_list initial.assigned_volunteers
                  |> List.map ~f:(fun { Rich.Volunteer.id; _ } ->
                      Rich.id_to_int id);
              })
            normalized_planning.quests
        in
        let () = Console.debug [ "TBN"; "Update assignations" ] in
        let* () = Assignations.remove_assignations ~solution:s_id_1 in
        let* () = Assignations.insert_assignations assignations in
        let open Fut.Syntax in
        let* res =
          let open Fut.Result_syntax in
          let* server_response =
            let open Brr_io.Fetch in
            let* data =
              Fut.return @@ Jsont_brr.encode Grist_import.data_jsont data
            in
            let body = Body.of_jstr data in
            Console.debug [ "TBN"; "Querying server" ];
            let method' = Jstr.v "PUT" in
            let uri = Jstr.v "https://localhost:5173/check-data" in
            let headers =
              Headers.of_assoc
                [ (Jstr.v "Content-Type", Jstr.v "application/json") ]
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
                Response.as_body resp |> Body.json
          in
          Fut.return
          @@ Jsont_brr.decode_jv Data_repr.Api.answer_jsont server_response
        in
        let open Fut.Result_syntax in
        match res with
        | Error jv ->
            let answer =
              rev_append_diags
                [ (Error, Jv.Error.message jv |> Jstr.to_string) ]
                initial_answer
            in
            Console.debug [ "TBN"; "Perform analysis on empty assignations." ];
            let analysis =
              Shared.Analysis.of_planning planning answer.solution
                normalized_planning
            in
            let state =
              { Tables.Solutions.data_rich = planning; answer; analysis }
            in
            Lwd.set App_state.last_answer (Some state);
            let* () = Solutions.upsert_solution s_id_1 state in
            Fut.ok (Console.error [ jv ])
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
              Shared.Analysis.of_planning planning answer.solution
                normalized_planning
            in
            let state =
              { Tables.Solutions.data_rich = planning; answer; analysis }
            in
            let () =
              match answer.status with
              | Feasible | Optimal ->
                  Lwd.set App_state.optimize_state (Ready (grist_data, state))
              | Api.Unknown | Api.ModelInvalid | Api.Infeasible ->
                  Lwd.set App_state.optimize_state Not_ready
            in
            let* () = Solutions.upsert_solution s_id_1 state in
            Fut.ok @@ Lwd.set App_state.last_answer (Some state)
      end

let fetch_last () =
  Logs.debug (fun m -> m "Fetching solution 1");
  let+ last_answer = Solutions.get_solution_1 () in
  Logs.debug (fun m -> m "Fetched solution 1");
  Lwd.set App_state.last_answer (Some last_answer)

let init_optimization_chart =
  let chart = ref None in
  fun canvas ->
    let open Chartjs in
    let options =
      let scales =
        [
          ( Jstr.v "x",
            Options.Scale.create
              ~title:(Jstr.v "Temps de recherche")
              ~typ:(Jstr.v "logarithmic") () );
          ( Jstr.v "y",
            Options.Scale.create ~title:(Jstr.v "Score") ~grid_display:false
              ~position:(Jstr.v "left") ~typ:(Jstr.v "linear") () );
          ( Jstr.v "y2",
            Options.Scale.create ~title:(Jstr.v "Satisfaction")
              ~position:(Jstr.v "right") ~typ:(Jstr.v "linear") ~min:(-1.)
              ~max:1. () );
        ]
      in
      Options.create ~responsive:true ~maintainAspectRatio:false
        ~animation:false ~scales ()
    in
    let d_objective =
      let data = Jv.of_jv_list [] in
      Dataset.create ~label:(Jstr.v "Score") ~border_color:(rgb 75 192 192)
        ~background_color:(rgba 75 192 192 0.2) ~tension:0.1 ~data ()
    in
    let d_satisfaction =
      let data = Jv.of_jv_list [] in
      Dataset.create ~label:(Jstr.v "Satisfaction")
        ~border_color:(Jstr.v "IndianRed") ~tension:0. ~point_radius:0
        ~y_axis_ID:(Jstr.v "y2") ~data ()
    in
    let data = Data.create ~datasets:[ d_objective; d_satisfaction ] () in
    let chart =
      match !chart with
      | None ->
          let c =
            Chart.create ~canvas ~chart_type:(Jstr.v "line") ~options ()
          in
          chart := Some c;
          c
      | Some c -> c
    in
    let () = Chart.set_data chart data in
    (chart, d_objective, d_satisfaction)

let optimize ~(chart_canvas : El.t) ~reuse grist_data
    (current_state : Solutions.t) =
  let planning = current_state.data_rich in
  let+ handle =
    let open Brr_io.Fetch in
    let* data =
      Fut.return @@ Jsont_brr.encode_jv Grist_import.data_jsont grist_data
    in
    let json =
      Jv.obj [| ("data", data); ("assignations", reuse) |] |> Json.encode
    in
    let body = Body.of_jstr json in
    let method' = Jstr.v "PUT" in
    let uri = Jstr.v "https://localhost:5173/optim" in
    let headers =
      Headers.of_assoc [ (Jstr.v "Content-Type", Jstr.v "application/json") ]
    in
    let init = Request.init ~body ~method' ~headers () in
    let () = Lwd.set App_state.optimize_state Running in
    let* response = url ~init uri in
    Response.as_body response |> Body.text
  in
  let module Event_source = Brr_io.Event_source in
  let url = Jstr.(append (v "https://localhost:5173/optim-stream/") handle) in
  let event_source = Event_source.create ~url () in
  let chart, d_objective, _d_satisfaction =
    init_optimization_chart chart_canvas
  in
  let mk_point time value =
    Jv.obj
      [| ("x", Jv.of_string (Float.to_string time)); ("y", Jv.of_float value) |]
  in
  (* TODO use App_state.normal ? *)
  let normalized_planning = Conv.normalize planning in
  let last_answer = ref None in
  let _ =
    let first = ref true in
    Ev.listen Ev.error
      (fun _ev ->
        if !first then begin
          first := false;
          match !last_answer with
          | None -> ()
          | Some json ->
              let answer =
                Jsont_brr.decode Data_repr.Api.answer_jsont json
                |> Result.get_ok
              in
              let analysis =
                Shared.Analysis.of_planning planning answer.solution
                  normalized_planning
              in
              let state = { current_state with answer; analysis } in
              Lwd.set App_state.last_answer (Some state);
              ignore
              @@
              let s_id_2 = 2 in
              let* () = Solutions.upsert_solution s_id_2 state in
              let assignations =
                List.map answer.solution
                  ~f:(Grist_import.Assignation.v ~solution:s_id_2)
              in
              let analysis =
                Shared.Analysis.of_planning planning answer.solution
                  normalized_planning
              in
              Lwd.set App_state.last_answer
                (Some { state with answer; analysis });
              let* () = Assignations.remove_assignations ~solution:s_id_2 in
              Assignations.insert_assignations assignations
        end)
      (Event_source.as_target event_source)
  in
  let handle_new_solution (answer_json : Jstr.t) =
    let time =
      Jsont_brr.decode
        Jsont.(path Path.(mem "user_time" root) Jsont.number)
        answer_json
      |> Result.get_ok
    in
    let objective_value =
      Jsont_brr.decode
        Jsont.(path Path.(mem "objective_value" root) Jsont.number)
        answer_json
      |> Result.get_ok
    in
    let best_objective_bound =
      Jsont_brr.decode
        Jsont.(path Path.(mem "best_objective_bound" root) Jsont.number)
        answer_json
      |> Result.get_ok
    in
    last_answer := Some answer_json;
    Chartjs.Dataset.push_data d_objective
      (mk_point time (objective_value -. best_objective_bound));
    Chartjs.Chart.update chart
  in
  (* let handle = Brrer.Limiter.throttle ~delay_ms:10_000 in *)
  let _ =
    Ev.listen Brr_io.Message.Ev.message
      (fun ev ->
        let json : Jstr.t = Brr_io.Message.Ev.data (Ev.as_type ev) in
        handle_new_solution json)
      (Event_source.as_target event_source)
  in
  Console.error [ "DBG"; "HANDLE"; handle; event_source ]

let app =
  let open Lwd_infix in
  let last_answer = Lwd.get App_state.last_answer in
  let controls =
    let chart_canvas, optimize_chart =
      let chart_canvas = El.canvas [] in
      let display =
        let$ state = Lwd.get App_state.optimize_state in
        match state with
        | Running -> At.void
        | _ -> At.style (Jstr.v "display: none")
      in
      ( chart_canvas,
        Pico_ui.Elwd.section
          ~at:[ `R display ]
          [
            `P
              (El.div ~at:[ At.style (Jstr.v "height:20rem") ] [ chart_canvas ]);
          ] )
    in
    let check_btn =
      let disabled =
        let$ state = Lwd.get App_state.check_btn in
        match state with `In_progress -> At.disabled | `Ready -> At.void
      in
      let ev =
        let$ state = Lwd.get App_state.check_btn in
        match state with
        | `In_progress -> Elwd.handler Ev.click (fun _ -> ())
        | `Ready ->
            Elwd.handler Ev.click (fun _ ->
                Lwd.set App_state.check_btn `In_progress;
                sat ()
                |> Fut.map (fun _ -> Lwd.set App_state.check_btn `Ready)
                |> ignore)
      in
      Elwd.button
        ~at:[ `R disabled ]
        ~ev:[ `R ev ]
        [ `P (El.txt' "1. Déplier les quêtes et vérifier la faisabilité") ]
    in
    let reuse_var = Lwd.var None in
    let reuse =
      let$* sol = Lwd.get App_state.selected_solution_in_grist in
      match sol with
      | None ->
          Lwd.set reuse_var None;
          Lwd.return (El.nbsp ())
      | Some sol ->
          let name = Solutions.name sol in
          let chk =
            Brr_lwd_ui.Forms.Field_checkboxes.make_single ~var:reuse_var
              {
                value = Solutions.id sol;
                id = "reuse_switch";
                name = "reuse_switch";
                label =
                  (fun () ->
                    [
                      `P
                        (El.txt'
                           ("Réutiliser la solution " ^ Jstr.to_string name));
                    ]);
                state = true;
              }
          in
          chk.element
    in
    let optimize_btn =
      let disabled =
        let$ state = Lwd.get App_state.optimize_state in
        match state with
        | Not_ready | Running -> At.disabled
        | Ready _ -> At.void
      in
      let ev =
        let$ state = Lwd.get App_state.optimize_state in
        match state with
        | Not_ready | Running -> Elwd.handler Ev.click (fun _ -> ())
        | Ready (grist_data, state) ->
            Elwd.handler Ev.click (fun _ ->
                ignore
                  begin
                    let reuse = Lwd.peek reuse_var in
                    let* assignations =
                      match reuse with
                      | None -> Fut.ok (Jv.Jarray.create 0)
                      | Some solution ->
                          let+ assignations =
                            Assignations.get_assignations ~solution
                          in
                          Jv.of_jv_list assignations
                    in
                    optimize ~chart_canvas grist_data ~reuse:assignations state
                  end)
      in
      Elwd.button
        ~at:[ `R disabled ]
        ~ev:[ `R ev ]
        [ `P (El.txt' "2. Optimiser") ]
    in
    let print_options_modal, show_modal =
      Print.modal (fun () ->
          match Lwd.peek App_state.last_answer with
          | None -> Fut.ok None
          | Some (a : Solutions.t) ->
              Fut.ok (Some (a.data_rich, a.answer.solution)))
    in
    let print_btn =
      let disabled =
        let$ answer = Lwd.get App_state.last_answer in
        match answer with
        | None | Some { answer = { solution = []; _ }; _ } -> At.disabled
        | Some _ -> At.void
      in
      let ev =
        let f = fun _ -> Lwd.set show_modal true in
        Elwd.handler Ev.click f
      in
      Elwd.button
        ~at:[ `R disabled ]
        ~ev:[ `P ev ]
        [ `P (El.txt' "3. Imprimer") ]
    in
    let btns =
      Elwd.fieldset
        ~at:[ `P (At.v (Jstr.v "role") (Jstr.v "group")) ]
        [ `R check_btn ]
    in
    let btns2 =
      Elwd.fieldset
        ~at:[ `P (At.v (Jstr.v "role") (Jstr.v "group")) ]
        [ `R optimize_btn; `R print_btn ]
    in
    Pico_ui.Elwd.section
      [ `R print_options_modal; `R btns; `R reuse; `R btns2; `R optimize_chart ]
  in
  let results =
    let title =
      let$ status = Lwd.get App_state.server_status in
      let slug = App_state.to_fr_slug status in
      El.txt' ("Résultats " ^ slug)
    in
    let results =
      let txt =
        Lwd.map last_answer ~f:(function
          | None -> El.txt' "En attente des premiers résultats."
          | Some
              {
                answer =
                  { status; sufficient_assumptions_for_infeasibility; date; _ };
                _;
              } ->
              let date =
                Zoned_datetime.to_local_datetime date |> Datetime.to_string
              in
              let sufass =
                match sufficient_assumptions_for_infeasibility with
                | [] -> []
                | ass ->
                    El.h4
                      [ El.txt' "Sufficient assumptions for infeasibility:" ]
                    :: [
                         El.ul (List.map ~f:(fun s -> El.li [ El.txt' s ]) ass);
                       ]
              in
              El.div
                (El.txt' (Ortools.Sat.Response.string_of_status status)
                :: El.txt' (" (fait à " ^ date ^ ")")
                :: sufass))
      in
      let diffs =
        let$* state = Lwd.get App_state.last_answer in
        match state with
        | None -> Lwd.return (El.nbsp ())
        | Some { analysis; _ } -> Diffs_table.make analysis
      in
      let by_volunteer_infos =
        let$* state = App_state.normal in
        match state with
        | None -> Lwd.return (El.nbsp ())
        | Some state ->
            Infos.per_volunteer_el state.data state.state.answer.solution
      in
      Pico_ui.Elwd.section
        [
          `R txt; `P (El.hr ()); `R by_volunteer_infos; `P (El.hr ()); `R diffs;
        ]
    in
    Pico_ui.accordion ~name:"results" ~title [ `R results ]
  in
  let diagnostics =
    let diags =
      let$ answer = Lwd.get App_state.last_answer in
      let diags =
        match answer with
        | None -> []
        | Some { answer; analysis; _ } ->
            List.rev_append (Shared.Analysis.diags analysis) answer.diagnostics
      in
      El.div
      @@
      if List.is_empty diags then
        [ Pico_ui.diag_card (Info, "Jusqu'ici tout va bien.") ]
      else List.map diags ~f:Pico_ui.diag_card
    in
    let section =
      let title = Lwd.return @@ El.txt' "Diagnostiques" in
      Pico_ui.accordion ~name:"diags" ~title [ `R diags ]
    in
    section
  in
  let analyses =
    let$* results = App_state.normal in
    match results with
    | None -> Lwd.return (El.nbsp ())
    | Some state -> Infos.capacity_table state
  in
  let available_volunteers =
    let$* results = Lwd.get App_state.last_answer in
    match results with
    | None -> Lwd.return (El.nbsp ())
    | Some { data_rich; _ } -> Infos.available_volunteers_widget data_rich
  in
  let analyses =
    Pico_ui.accordion ~name:"analyses"
      ~title:(Lwd.return (El.txt' "Outils et analyses"))
      [
        `P
          (El.blockquote
             [
               El.txt'
                 "Ces données sont approximatives et ne tiennent pas compte de \
                  toutes les contraintes.";
             ]);
        `P (El.h4 [ El.txt' "Compteur de bénévoles" ]);
        `R available_volunteers;
        `P (El.h4 [ El.txt' "Main d'oeuvre requise / disponible" ]);
        `R analyses;
      ]
  in
  (* let dbg_planning =
    Lwd.map (Lwd.get App_state.last_answer) ~f:(function
      | Some { App_state.data_rich; answer; _ } ->
          Render.make_plannings data_rich answer [ `By_place ]
      | None -> El.nbsp ())
  in *)
  Elwd.div [ `R controls; `R diagnostics; `R analyses; `R results ]

let on_record () =
  let f = fun v -> Lwd.set App_state.selected_solution_in_grist (Some v) in
  let callback = Jv.callback ~arity:1 f in
  let options =
    Jv.obj [| ("keepEncoded", Jv.false'); ("expandRefs", Jv.false') |]
  in
  Grist.on_record ~callback ~options ()

let _ =
  let on_load _ =
    let _ = fetch_last () in
    let _ = on_record () in
    let root = El.find_first_by_selector (Jstr.v "main") |> Option.get in
    let app = Lwd.observe app in
    let f _ = ignore @@ Lwd.quick_sample app in
    let on_invalidate _ = ignore @@ G.request_animation_frame f in
    El.append_children root [ Lwd.quick_sample app ];
    Lwd.set_on_invalidate app on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)
