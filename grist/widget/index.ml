open Brr
open Fut.Result_syntax
open! Data_repr

let infos_tbl_id = Jstr.v "Infos_generales"
let options_tbl_id = Jstr.v "Options_du_solveur"
let places_tbl_id = Jstr.v "Lieux"
let task_types_tbl_id = Jstr.v "Types_de_quete"
let time_slots_tbl_id = Jstr.v "Plages_horaires_ponctuelles"
let volunteers_tbl_id = Jstr.v "Benevoles"
let quests_tbl_id = Jstr.v "Quetes"
let assignations_tbl_id = Jstr.v "Assignations"

let fetch table_id =
  let open Grist in
  let+ result = Doc_API.fetch_table ~table_id in
  Data.Row_records.by_row result

let sat =
  let last_data = ref None in
  fun () ->
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
    match (Jsont_brr.decode Grist_import.data_jsont data_json, !last_data) with
    | Ok data, Some last when Equal.poly last data ->
        Console.debug [ "DBG"; "Nothing to do" ];
        Fut.return (Ok ())
    | Ok data, _ ->
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
          Grist.Table_operations.upsert assignations_table ~records ()
        in
        let+ res =
          let open Brr_io.Fetch in
          let body = Body.of_jstr data_json in

          let method' = Jstr.v "PUT" in
          let uri = Jstr.v "/grist/data" in
          let headers =
            Headers.of_assoc
              [ (Jstr.v "Content-Type", Jstr.v "application/json") ]
          in
          let init = Request.init ~body ~method' ~headers () in
          let* response = url ~init uri in
          Response.as_body response |> Body.json
        in
        let elt = El.find_first_by_selector (Jstr.v "#readout") |> Option.get in
        let j = Jv.get Jv.global "JSON" in
        let str =
          Jv.call j "stringify" [| res; Jv.null; Jv.of_int 2 |] |> Jv.to_jstr
        in
        El.set_children elt
        @@ List.map ~f:(fun s -> El.pre [ El.txt s ])
        @@ Jstr.cuts ~sep:(Jstr.v "\\n") str
    | Error err, _ ->
        Console.error [ "Decoding error: "; Jv.Error.message err ];
        Fut.return (Ok ())

let _ =
  sat ();
  Brr.G.set_interval ~ms:2000 sat
