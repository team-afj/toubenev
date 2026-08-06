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

  let table () =
    Lazy.force (lazy (Grist.get_table ~table_id:Data.solutions_tbl_id ()))

  let of_jv obj =
    (Jv.Int.get obj "id", Jv.Jstr.get obj "name", Jv.get obj "last_answer")

  let ls () =
    let+ solutions = Data.fetch Data.solutions_tbl_id in
    Jv.to_list of_jv solutions

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

  let get_solution_1 () =
    let* solutions = Data.fetch Data.solutions_tbl_id in
    let first = Jv.call solutions "at" [| Jv.of_int 0 |] in
    let answer = Jv.get first "last_answer" in
    Fut.return @@ Jsont_brr.decode jsont (Jv.to_jstr answer)
end
