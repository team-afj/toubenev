open Brrer
open Brr
open Brr_lwd
open Fut.Result_syntax
open Lunar_jsont
open Shared
open! Data_repr

(* TODO: There might be more efficient way to do some tthings by using the REST
API with short-live tokens. *)

(* Current (26/05/2026) dataflow:
  -- Fetch from Grist widget API -> [Grist_import.data]
     -- [Sent to the server fot sat check ] -> [Api.answer]
  -- [Grist_import.to_planning] -> [Rich.Planning.t]
  -- [Conv.normalize] -> [Api.data]
     -- Used for "Dummy assignations" -> [Grist_import.Assignation.t list]
*)

module App = struct
  let diagnostics : Api.diagnostic list Lwd.var = Lwd.var []

  type last_state = Grist_import.data * Api.answer [@@deriving jsont]

  let last_answer : last_state option Lwd.var = Lwd.var None
  let analyses : Shared.Analysis.t option Lwd.var = Lwd.var None
  let check_btn : [ `Ready | `In_progress ] Lwd.var = Lwd.var `Ready

  type optimize_state = Not_ready | Ready of Grist_import.data | Running

  let optimize_state : optimize_state Lwd.var = Lwd.var Not_ready

  type server_status = Offline | Working | Done

  let to_fr_slug = function Offline -> "⛓️‍💥" | Working -> "🤖" | Done -> "🔗"

  let server_status : server_status Lwd.var = Lwd.var Offline
end

module Data = struct
  let infos_tbl_id = Jstr.v "Infos_generales"
  let options_tbl_id = Jstr.v "Options_du_solveur"
  let places_tbl_id = Jstr.v "Lieux"
  let task_types_tbl_id = Jstr.v "Types_de_quetes"
  let time_slots_tbl_id = Jstr.v "Plages_horaires_ponctuelles"
  let volunteers_tbl_id = Jstr.v "Benevoles"
  let quests_tbl_id = Jstr.v "Quetes"
  let solutions_tbl_id = Jstr.v "Solutions"
  let assignations_tbl_id = Jstr.v "Assignations"

  let fetch table_id =
    let open Grist in
    let+ result = Doc_API.fetch_table ~table_id in
    Data.Row_records.by_row result

  let fetch_all () =
    let* infos = fetch infos_tbl_id in
    let* options = fetch options_tbl_id in
    let* places = fetch places_tbl_id in
    let* task_types = fetch task_types_tbl_id in
    let* time_specs = fetch time_slots_tbl_id in
    let* volunteers = fetch volunteers_tbl_id in
    let* quests = fetch quests_tbl_id in
    let data_json =
      Jv.obj
        [|
          ("infos", infos);
          ("options", options);
          ("places", places);
          ("task_types", task_types);
          ("time_specs", time_specs);
          ("volunteers", volunteers);
          ("quests", quests);
        |]
      |> Json.encode
    in
    Fut.return @@ Jsont_brr.decode Grist_import.data_jsont data_json
end

module Titles = struct
  (* TODO: This is kinda hackish since we modify a internal, undocumented,
     table. It can break anytime. *)
  let table_id = Jstr.v "_grist_Views_section"
  let widget_base_name = "Solver link"
  let widget_base_name_j = Jstr.v "Solver link"

  (* This function updates the title of item number [id]
     in "_grist_Views_section" *)
  let meta_update_title ~ids new_title =
    let open Grist in
    let views_section_table = get_table ~table_id () in
    let records =
      let title = Jstr.v "title" in
      let new_title = Jv.of_string new_title in
      List.map ids ~f:(fun id ->
          Record.v ~id ~fields:[| (title, new_title) |] ())
    in
    Table_operations.update views_section_table ~records ()

  (* List all uses the the widget by searching in the section titles in
     [_grist_Views_section].
     TODO: this is not very robust. *)
  let all_widget_uses () =
    let+ rows = Data.fetch table_id in
    Jv.to_list
      (fun row ->
        let id = Jv.get row "id" |> Jv.to_int in
        let title_jv = Jv.get row "title" in
        if Jv.is_null title_jv || Jv.is_undefined title_jv then None
        else if
          Jv.call title_jv "includes" [| Jv.of_jstr widget_base_name_j |]
          |> Jv.to_bool
        then Some id
        else None)
      rows
    |> List.filter_map ~f:Fun.id

  let update_prefixes prefix =
    Console.error [ "DBG4"; all_widget_uses () ];
    let* ids = all_widget_uses () in
    meta_update_title ~ids @@ prefix ^ " " ^ widget_base_name
end

module Solutions = struct
  let table () =
    Lazy.force (lazy (Grist.get_table ~table_id:Data.solutions_tbl_id ()))

  let upsert_solution_1 data answer =
    let* json =
      Fut.return @@ Jsont_brr.encode App.last_state_jsont (data, answer)
    in
    let records =
      [
        Grist.Record.v ~id:1
          ~fields:[| (Jstr.v "last_answer", Jv.of_jstr json) |]
          ();
      ]
    in
    Grist.Table_operations.update (table ()) ~records ()

  let get_solution_1 () =
    let* solutions = Data.fetch Data.solutions_tbl_id in
    let first = Jv.call solutions "at" [| Jv.of_int 0 |] in
    let answer = Jv.get first "last_answer" in
    Fut.return @@ Jsont_brr.decode App.last_state_jsont (Jv.to_jstr answer)
end

module Assignations = struct
  let assignations_table () =
    Lazy.force (lazy (Grist.get_table ~table_id:Data.assignations_tbl_id ()))

  let remove_assignations ~solution =
    let* current_assignations = Data.(fetch assignations_tbl_id) in
    let current_assignations =
      Jv.to_list
        (fun obj ->
          (Jv.get obj "id" |> Jv.to_int, Jv.get obj "solution" |> Jv.to_int))
        current_assignations
    in
    let solution_1_assignations =
      List.filter_map
        ~f:(function id, s when s = solution -> Some id | _ -> None)
        current_assignations
    in
    let record_ids = solution_1_assignations in
    Grist.Table_operations.destroy (assignations_table ()) ~record_ids

  let insert_assignations assignations =
    let open Grist in
    let records =
      List.map assignations ~f:(fun (a : Grist_import.Assignation.t) ->
          let list to_jv l =
            if List.is_empty l then Jv.null
            else Jv.of_jv_list (Jv.of_string "L" :: List.map ~f:to_jv l)
          in
          let fields =
            [|
              (Jstr.v "volunteers", list Jv.of_int a.volunteers);
              (Jstr.v "start", Jv.of_int a.start);
              (Jstr.v "end_", Jv.of_int a.end_);
            |]
          in
          let require =
            [|
              (Jstr.v "ref", Jv.of_string a.ref);
              (Jstr.v "solution", Jv.of_int a.solution);
              (Jstr.v "initial_quest", Jv.of_int a.initial_quest);
            |]
          in
          Add_or_update_record.v ~require ~fields ())
    in
    Grist.Table_operations.upsert (assignations_table ()) ~records ()
end

let sat =
  let last_data = ref None in
  fun () ->
    Fut.bind (Data.fetch_all ()) @@ fun data ->
    match (data, !last_data) with
    | Error err, _ ->
        Console.error [ "DBG"; "Decoding error: "; Jv.Error.message err ];
        Fut.return (Ok ())
    | Ok data, Some last when Equal.poly last data ->
        Lwd.set App.optimize_state (Ready data);
        Fut.return (Ok ())
    | Ok data, _ -> begin
        let () = Lwd.set App.optimize_state Not_ready in
        let () = Console.debug [ "DBG"; "Data changes" ] in
        let () = last_data := Some data in
        let _id_map, planning = Grist_import.to_planning data in
        let () = Console.debug [ "DBG"; "Normalize" ] in
        let normalized_planning = Conv.normalize planning in
        (* Analysis *)
        let analysis_diagnostics =
          let result =
            Shared.Analysis.of_planning planning normalized_planning
          in
          Lwd.set App.analyses (Some result);
          Analysis.diags result
        in
        let () =
          Lwd.set App.diagnostics
            (List.rev_append analysis_diagnostics
               normalized_planning.diagnostics)
        in
        (* New assignations (unfolded quests) *)
        let () = Console.debug [ "DBG"; "Prepare empty assignations" ] in
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
                Grist_import.Assignation.solution = 1;
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
        let* () = Assignations.remove_assignations ~solution:1 in
        let* () = Assignations.insert_assignations assignations in
        let* res =
          let open Brr_io.Fetch in
          let* data =
            Fut.return @@ Jsont_brr.encode Grist_import.data_jsont data
          in
          let body = Body.of_jstr data in
          Console.error [ "DBG"; "Querying server  " ];
          let method' = Jstr.v "PUT" in
          let uri = Jstr.v "http://localhost:1357/grist/data" in
          let headers =
            Headers.of_assoc
              [ (Jstr.v "Content-Type", Jstr.v "application/json") ]
          in
          let init = Request.init ~body ~method' ~headers () in
          let open Fut.Syntax in
          let () = Lwd.set App.server_status Working in
          let* response = url ~init uri in
          match response with
          | Error err ->
              let* _ = Titles.update_prefixes "⛓️‍💥" in
              let () = Lwd.set App.server_status Offline in
              Fut.error err
          | Ok resp ->
              let () = Lwd.set App.server_status Done in
              Response.as_body resp |> Body.json
        in
        match Jsont_brr.decode_jv Data_repr.Api.answer_jsont res with
        | Error jv -> Fut.ok (Console.error [ jv ])
        | Ok answer ->
            let () =
              Lwd.set App.diagnostics
                (List.rev_append answer.diagnostics @@ Lwd.peek App.diagnostics)
            in
            let* () = Solutions.upsert_solution_1 data answer in
            let* () =
              match answer.status with
              | Feasible | Optimal ->
                  let () = Lwd.set App.optimize_state (Ready data) in
                  Titles.update_prefixes "🟢"
              | Unknown | ModelInvalid | Infeasible ->
                  let () = Lwd.set App.optimize_state Not_ready in
                  Titles.update_prefixes "🔴"
            in
            Fut.ok @@ Lwd.set App.last_answer (Some (data, answer))
      end

let fetch_last () =
  let+ last_answer = Solutions.get_solution_1 () in
  Lwd.set App.last_answer (Some last_answer)

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

let optimize ~(chart_canvas : El.t) (data : Grist_import.data) =
  let+ handle =
    let open Brr_io.Fetch in
    let* json = Fut.return @@ Jsont_brr.encode Grist_import.data_jsont data in
    let body = Body.of_jstr json in
    let method' = Jstr.v "PUT" in
    let uri = Jstr.v "http://localhost:1357/grist/optim" in
    let headers =
      Headers.of_assoc [ (Jstr.v "Content-Type", Jstr.v "application/json") ]
    in
    let init = Request.init ~body ~method' ~headers () in
    let () = Lwd.set App.optimize_state Running in
    let* response = url ~init uri in
    Response.as_body response |> Body.text
  in
  let module Event_source = Brr_io.Event_source in
  let url = Jstr.(append (v "http://localhost:1357/optim-stream/") handle) in
  let event_source = Event_source.create ~url () in
  let chart, d_objective, d_satisfaction =
    init_optimization_chart chart_canvas
  in
  let mk_point time value =
    Jv.obj
      [| ("x", Jv.of_string (Float.to_string time)); ("y", Jv.of_float value) |]
  in
  let _ =
    let first = ref true in
    Ev.listen Ev.error
      (fun _ev ->
        if !first then begin
          first := false;
          match Lwd.peek App.last_answer with
          | None -> ()
          | Some (data, answer) ->
              ignore
              @@
              let* () = Solutions.upsert_solution_1 data answer in
              let assignations =
                List.map answer.solution
                  ~f:(Grist_import.Assignation.v ~solution:1)
              in
              Assignations.insert_assignations assignations
        end)
      (Event_source.as_target event_source)
  in
  let _ =
    Ev.listen Brr_io.Message.Ev.message
      (fun ev ->
        let json : Jstr.t = Brr_io.Message.Ev.data (Ev.as_type ev) in
        let answer =
          Jsont_brr.decode Data_repr.Api.answer_jsont json |> Result.get_ok
        in
        let time = answer.deterministic_time in
        let satisfaction = Api.satisfaction answer.solution in
        Lwd.set App.last_answer (Some (data, answer));
        Chartjs.Dataset.push_data d_objective
          (mk_point time
             (answer.objective_value -. answer.best_objective_bound));
        Chartjs.Dataset.push_data d_satisfaction (mk_point time satisfaction);
        Chartjs.Chart.update chart)
      (Event_source.as_target event_source)
  in
  Console.error [ "DBG"; "HANDLE"; handle; event_source ]

module Ui = struct
  let accordion ~name ?(closed = false) ~title content =
    (* TODO there might a bug in lwd, with the pure version of this function the
    open attribute disapears *)
    let at =
      let at = [ `P (At.name (Jstr.v name)) ] in
      match closed with
      | true -> at
      | false -> `P (At.v (Jstr.v "open") (Jstr.v "open")) :: at
    in
    Elwd.details ~at
      (`R (Elwd.summary [ `R title ]) :: [ `R (Pico_ui.Elwd.section content) ])

  let diag_card ((lvl, msg) : Api.diagnostic) =
    let at =
      [
        At.class' (Jstr.v "diag-card");
        At.class' (Jstr.v (Api.diagnostic_level_to_string lvl));
      ]
    in
    El.article ~at [ El.txt' msg ]
end

let app =
  let open Lwd_infix in
  let last_answer = Lwd.get App.last_answer in
  let controls =
    let chart_canvas, optimize_chart =
      let chart_canvas = El.canvas [] in
      let display =
        let$ state = Lwd.get App.optimize_state in
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
        let$ state = Lwd.get App.check_btn in
        match state with `In_progress -> At.disabled | `Ready -> At.void
      in
      let ev =
        let$ state = Lwd.get App.check_btn in
        match state with
        | `In_progress -> Elwd.handler Ev.click (fun _ -> ())
        | `Ready ->
            Elwd.handler Ev.click (fun _ ->
                Lwd.set App.check_btn `In_progress;
                sat ()
                |> Fut.map (fun _ -> Lwd.set App.check_btn `Ready)
                |> ignore)
      in
      Elwd.button
        ~at:[ `R disabled ]
        ~ev:[ `R ev ]
        [ `P (El.txt' "1. Déplier les quêtes et vérifier la faisabilité") ]
    in
    let optimize_btn =
      let disabled =
        let$ state = Lwd.get App.optimize_state in
        match state with
        | Not_ready | Running -> At.disabled
        | Ready _ -> At.void
      in
      let ev =
        let$ state = Lwd.get App.optimize_state in
        match state with
        | Not_ready | Running -> Elwd.handler Ev.click (fun _ -> ())
        | Ready data ->
            Elwd.handler Ev.click (fun _ ->
                ignore (optimize ~chart_canvas data))
      in
      Elwd.button
        ~at:[ `R disabled ]
        ~ev:[ `R ev ]
        [ `P (El.txt' "2. Optimiser") ]
    in
    let print_btn =
      let ev =
        Elwd.handler Ev.click @@ fun _ ->
        match Lwd.peek App.last_answer with
        | None -> ()
        | Some (data, answer) ->
            let _id_map, planning = Grist_import.to_planning data in
            let planning = Render.make_plannings planning answer in
            Print.print planning
      in
      Elwd.button ~ev:[ `P ev ] [ `P (El.txt' "3. Imprimer") ]
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
    Pico_ui.Elwd.section [ `R btns; `R btns2; `R optimize_chart ]
  in
  let results =
    let txt =
      Lwd.map last_answer ~f:(function
        | None -> El.txt' "En attente des premiers résultats."
        | Some (_, { status; sufficient_assumptions_for_infeasibility; date; _ })
          ->
            let date =
              Zoned_datetime.to_local_datetime date |> Datetime.to_string
            in
            let sufass =
              match sufficient_assumptions_for_infeasibility with
              | [] -> []
              | ass ->
                  El.h4 [ El.txt' "Sufficient assumptions for infeasibility:" ]
                  :: [ El.ul (List.map ~f:(fun s -> El.li [ El.txt' s ]) ass) ]
            in
            El.div
              (El.txt' (Ortools.Sat.Response.string_of_status status)
              :: El.txt' (" (fait à " ^ date ^ ")")
              :: sufass))
    in
    Pico_ui.Elwd.section [ `R txt ]
  in
  let results =
    let title =
      let$ status = Lwd.get App.server_status in
      let slug = App.to_fr_slug status in
      El.txt' ("Résultats " ^ slug)
    in
    Ui.accordion ~name:"results" ~title [ `R results ]
  in
  let diagnostics =
    let diags =
      let$ diags = Lwd.get App.diagnostics in
      El.div
      @@
      if List.is_empty diags then
        [ Ui.diag_card (Info, "Jusqu'ici tout va bien.") ]
      else List.map diags ~f:Ui.diag_card
    in
    let section =
      let title = Lwd.return @@ El.txt' "Diagnostiques" in
      Ui.accordion ~name:"diags" ~title [ `R diags ]
    in
    section
  in
  let analyses =
    let$ results = Lwd.get App.analyses in
    match results with
    | None -> El.nbsp ()
    | Some { daily } ->
        let th ?tooltip v =
          let el =
            let txt = El.txt' v in
            match tooltip with
            | None -> txt
            | Some tip -> El.abbr ~at:[ At.title (Jstr.v tip) ] [ txt ]
            (* El.em ~at:[ At.v (Jstr.v "data-tooltip") (Jstr.v tip) ] [ txt ] *)
          in
          El.th [ el ]
        in
        let td ?at v = El.td ?at [ El.txt' v ] in
        let d_to_string d =
          let h, m, s = Duration.hms d in
          Printf.sprintf "%02d:%02d:%02d" h m s
        in
        let jours =
          List.rev
          @@ Date.Map.fold
               (fun d
                    {
                      Analysis.total_quest_time;
                      total_volunteer_time;
                      max_concurrent_volunteers;
                      available_volunteers;
                    } acc ->
                 let at =
                   if Duration.(total_volunteer_time < total_quest_time) then
                     Some [ At.class' (Jstr.v "warn") ]
                   else None
                 in
                 let at_av =
                   if available_volunteers < max_concurrent_volunteers then
                     Some [ At.class' (Jstr.v "error") ]
                   else None
                 in
                 El.tr
                   [
                     td (Date.to_string d);
                     td (d_to_string total_quest_time);
                     td ?at (d_to_string total_volunteer_time);
                     td (Int.to_string max_concurrent_volunteers);
                     td ?at:at_av (Int.to_string available_volunteers);
                   ]
                 :: acc)
               daily []
        in
        let totals =
          let total_q, total_v =
            Date.Map.fold
              (fun _d { Analysis.total_quest_time; total_volunteer_time; _ }
                   (acc_q, acc_v) ->
                Duration.(acc_q + total_quest_time, acc_v + total_volunteer_time))
              daily
              (Duration.zero, Duration.zero)
          in
          [ td "Total"; td (d_to_string total_q); td (d_to_string total_v) ]
        in
        Pico_ui.El.section
          [
            El.thead
              [
                El.tr
                  [
                    th "Jour";
                    th ~tooltip:"Durée totale des quêtes à accomplir" "⏱️ quêtes";
                    th ~tooltip:"Temps de bénévolat disponible" "⏱️👷‍♀️";
                    th
                      ~tooltip:
                        "Nombre maximum de bénévoles devant effecter une tâche \
                         au même moment."
                      "Max 👷‍♀️";
                    th
                      ~tooltip:
                        "Nombre de bénévoles disponibles. Si < à \"Max 👷‍♀️\",\n\
                         le planning est impossible à résoudre."
                      "#👷‍♀️";
                  ];
              ];
            El.tbody jours;
            El.tfoot totals;
          ]
  in
  let analyses =
    Ui.accordion ~name:"analyses"
      ~title:(Lwd.return (El.txt' "Analyses"))
      [
        `P
          (El.blockquote
             [
               El.txt'
                 "Ces données sont approximatives et d'autres contraintes \
                  peuvent empêcher de trouver un planning.";
             ]);
        `R analyses;
      ]
  in
  Elwd.div [ `R controls; `R results; `R diagnostics; `R analyses ]

let _ =
  let on_load _ =
    let _ = fetch_last () in
    let root = El.find_first_by_selector (Jstr.v "main") |> Option.get in
    let app = Lwd.observe app in
    let f _ = ignore @@ Lwd.quick_sample app in
    let on_invalidate _ = ignore @@ G.request_animation_frame f in
    El.append_children root [ Lwd.quick_sample app ];
    Lwd.set_on_invalidate app on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)
