open Brr
open Fut.Result_syntax
open! Data_repr

let debug = false

module Data = struct
  let infos_tbl_id = Jstr.v "Infos_generales"
  let options_tbl_id = Jstr.v "Options_du_solveur"
  let places_tbl_id = Jstr.v "Lieux"
  let task_types_tbl_id = Jstr.v "Types_de_quetes"
  let time_slots_tbl_id = Jstr.v "Plages_horaires_ponctuelles"
  let volunteers_tbl_id = Jstr.v "Benevoles"
  let breaks_tbl_id = Jstr.v "Breaks"
  let quests_groups_tbl_id = Jstr.v "Quests_groups"
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
    let* breaks = fetch breaks_tbl_id in
    let* quests_groups = fetch quests_groups_tbl_id in
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
          ("breaks", breaks);
          ("quests_groups", quests_groups);
          ("quests", quests);
        |]
      |> Json.encode
    in
    if debug then Console.debug [ "DBG"; "Fetched data: "; data_json ];
    Fut.return @@ Jsont_brr.decode Grist_import.data_jsont data_json
end

module Solutions = struct
  type t = {
    data : Grist_import.data;
    data_rich : Rich.Planning.t;
    answer : Api.answer;
    analysis : Shared.Analysis.t;
  }
  [@@deriving jsont]

  type normal = {
    state : t;
    data : Api.data;
    static_analysis : Static_analysis.with_cache;
  }

  let analysis_path =
    Jsont.(path (Path.mem "analysis" Path.root) Shared.Analysis.jsont)

  let data_rich_path =
    Jsont.(path (Path.mem "data_rich" Path.root) Rich.Planning.jsont)

  let data_rich jv =
    let answer = Jv.Jstr.get jv "last_answer" in
    Jsont_brr.decode data_rich_path answer

  let analysis jv =
    let answer = Jv.Jstr.get jv "last_answer" in
    Jsont_brr.decode analysis_path answer

  let table () =
    Lazy.force (lazy (Grist.get_table ~table_id:Data.solutions_tbl_id ()))

  let of_jv obj =
    (Jv.Int.get obj "id", Jv.Jstr.get obj "name", Jv.get obj "last_answer")

  let ls () =
    let+ solutions = Data.fetch Data.solutions_tbl_id in
    Jv.to_list of_jv solutions

  let create ~name state =
    let records =
      [
        Grist.New_record.v
          ~fields:
            [|
              (Jstr.v "name", Jv.of_jstr name);
              (Jstr.v "last_answer", Jv.of_jstr state);
            |]
          ();
      ]
    in
    Grist.Table_operations.create (table ()) ~records ()

  let remove ~solution =
    let record_ids = [ solution ] in
    Grist.Table_operations.destroy (table ()) ~record_ids

  let upsert_solution_1 state =
    let* json = Fut.return @@ Jsont_brr.encode jsont state in
    let records =
      [
        Grist.Record.v ~id:1
          ~fields:[| (Jstr.v "last_answer", Jv.of_jstr json) |]
          ();
      ]
    in
    Grist.Table_operations.update (table ()) ~records ()

  let get_solution i =
    let+ solutions = Data.fetch Data.solutions_tbl_id in
    let first = Jv.call solutions "at" [| Jv.of_int i |] in
    first

  let get_solution_1 () =
    let* s = get_solution 0 in
    let answer = Jv.get s "last_answer" in
    Fut.return @@ Jsont_brr.decode jsont (Jv.to_jstr answer)
end

module Assignations = struct
  let assignations_table () =
    Lazy.force (lazy (Grist.get_table ~table_id:Data.assignations_tbl_id ()))

  let get_assignations ~solution =
    let+ current_assignations = Data.(fetch assignations_tbl_id) in
    let current_assignations = Jv.to_jv_list current_assignations in
    List.filter
      ~f:(fun obj -> Jv.Int.get obj "solution" = solution)
      current_assignations

  let remove_assignations ~solution =
    let* current_assignations = get_assignations ~solution in
    let solution_1_assignations =
      List.map ~f:(fun obj -> Jv.get obj "id" |> Jv.to_int) current_assignations
    in
    let record_ids = solution_1_assignations in
    Grist.Table_operations.destroy (assignations_table ()) ~record_ids

  let create_solution_assignations ~solution assignations =
    let records =
      List.map assignations
        ~f:(fun
            ( initial_quest,
              ref,
              start,
              end_,
              volunteers,
              assigned_volunteers,
              keep )
          ->
          Grist.Add_or_update_record.v
            ~require:
              [|
                (Jstr.v "ref", ref);
                (Jstr.v "solution", Jv.of_int solution);
                (Jstr.v "initial_quest", initial_quest);
              |]
            ~fields:
              (Array.filter
                 ~f:(fun (_, v) -> not (Jv.is_none v))
                 [|
                   (Jstr.v "start", start);
                   (Jstr.v "end_", end_);
                   (Jstr.v "volunteers", volunteers);
                   (Jstr.v "assigned_volunteers", assigned_volunteers);
                   (Jstr.v "keep", keep);
                 |])
            ())
    in
    Grist.Table_operations.upsert (assignations_table ()) ~records ()

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
