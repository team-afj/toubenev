open Brr
open Brr_lwd
open Fut.Result_syntax
open! Data_repr

let last_answer : Api.answer option Lwd.var = Lwd.var None
let infos_tbl_id = Jstr.v "Infos_generales"
let options_tbl_id = Jstr.v "Options_du_solveur"
let places_tbl_id = Jstr.v "Lieux"
let task_types_tbl_id = Jstr.v "Types_de_quetes"
let time_slots_tbl_id = Jstr.v "Plages_horaires_ponctuelles"
let volunteers_tbl_id = Jstr.v "Benevoles"
let quests_tbl_id = Jstr.v "Quetes"
let assignations_tbl_id = Jstr.v "Assignations"

let fetch table_id =
  let open Grist in
  let+ result = Doc_API.fetch_table ~table_id in
  Data.Row_records.by_row result

module Titles = struct
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
    let+ rows = fetch table_id in
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

let sat =
  let last_data = ref None in
  fun () ->
    Console.debug [ "DBG"; "Fetch data" ];
    ignore
    @@
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
    Console.debug [ "DBG"; "Data fetched"; data_json ];
    match (Jsont_brr.decode Grist_import.data_jsont data_json, !last_data) with
    | Ok data, Some last when Equal.poly last data ->
        Console.debug [ "DBG"; "Nothing to do" ];
        Fut.return (Ok ())
    | Ok data, _ -> begin
        let () = Console.debug [ "DBG"; "Data changes" ] in
        let () = last_data := Some data in
        let _id_map, planning = Grist_import.to_planning data in
        let () = Console.debug [ "DBG"; "Normalize" ] in
        let normalized_planning = Conv.normalize planning in
        (* New assignations *)
        let () = Console.debug [ "DBG"; "Prepare empty assignations" ] in
        let assignations =
          let open Lunar in
          let open Normal in
          Quests.to_list_map
            ~f:(fun { Quest.id; name; initial; slot; _ } ->
              let initial_quest = Rich.id_to_int initial.id in
              let start =
                Datetime.to_duration slot.start |> Duration.to_seconds
              in
              let end_ =
                Datetime.(slot.start + slot.duration)
                |> Datetime.to_duration |> Duration.to_seconds
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
        let assignations_table =
          Grist.get_table ~table_id:assignations_tbl_id ()
        in
        let* _ =
          (* Remove old assignations for solution 0 *)
          let+ current_assignations = fetch assignations_tbl_id in
          let current_assignations =
            Jv.to_list
              (fun obj ->
                ( Jv.get obj "id" |> Jv.to_int,
                  Jv.get obj "solution" |> Jv.to_int ))
              current_assignations
          in
          let solution_1_assignations =
            List.filter_map
              ~f:(function id, 1 -> Some id | _ -> None)
              current_assignations
          in
          let record_ids = solution_1_assignations in
          Grist.Table_operations.destroy assignations_table ~record_ids
        in
        let* () =
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
                Console.error [ "DBG"; require ];
                Add_or_update_record.v ~require ~fields ())
          in
          Console.error [ "DBG"; "Upsert assignations" ];
          Grist.Table_operations.upsert assignations_table ~records ()
        in
        let* res =
          let open Brr_io.Fetch in
          let body = Body.of_jstr data_json in
          Console.error [ "DBG"; "Querying server  " ];
          let method' = Jstr.v "PUT" in
          let uri = Jstr.v "http://localhost:1357/grist/data" in
          let headers =
            Headers.of_assoc
              [ (Jstr.v "Content-Type", Jstr.v "application/json") ]
          in
          let init = Request.init ~body ~method' ~headers () in
          let open Fut.Syntax in
          let* response = url ~init uri in
          match response with
          | Error err ->
              let* _ = Titles.update_prefixes "⛓️‍💥" in
              Fut.error err
          | Ok resp -> Response.as_body resp |> Body.json
        in
        match Jsont_brr.decode_jv Data_repr.Api.answer_jsont res with
        | Error jv -> Fut.ok (Console.error [ jv ])
        | Ok answer ->
            let* () =
              match answer.status with
              | Feasible | Optimal -> Titles.update_prefixes "🟢"
              | Unknown | ModelInvalid | Infeasible ->
                  Titles.update_prefixes "🔴"
            in
            Fut.ok @@ Lwd.set last_answer (Some answer)
      end
    | Error err, _ ->
        Console.error [ "DBG"; "Decoding error: "; Jv.Error.message err ];
        Fut.return (Ok ())

let _ =
  sat ();
  Brr.G.set_interval ~ms:2000 sat

let app =
  let status = Lwd.get last_answer in
  let txt =
    Lwd.map status ~f:(function
      | None -> El.txt' "En attente des premiers résultats."
      | Some { status; sufficient_assumptions_for_infeasibility; _ } ->
          El.div
            [
              El.txt' @@ Ortools.Sat.Response.string_of_status status;
              El.br ();
              El.txt' "Sufficient assumptions for infeasibility:";
              El.br ();
              El.txt' sufficient_assumptions_for_infeasibility;
            ])
  in
  Elwd.div [ `R txt ]

let _ =
  let on_load _ =
    let app = Lwd.observe app in
    let f _ = ignore @@ Lwd.quick_sample app in
    let on_invalidate _ = ignore @@ G.request_animation_frame f in
    El.append_children (Document.body G.document) [ Lwd.quick_sample app ];
    Lwd.set_on_invalidate app on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)
