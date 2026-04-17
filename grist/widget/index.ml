open Brr
open Fut.Result_syntax

let infos_tbl_id = Jstr.v "Infos_generales"
let options_tbl_id = Jstr.v "Options_du_solveur"
let places_tbl_id = Jstr.v "Lieux"
let task_types_tbl_id = Jstr.v "Types_de_quete"
let time_slots_tbl_id = Jstr.v "Plages_horaires_ponctuelles"
let volunteers_tbl_id = Jstr.v "Benevoles"
let quests_tbl_id = Jstr.v "Quetes"

let fetch table_id =
  let open Grist in
  let+ result = Doc_API.fetch_table ~table_id in
  Data.Row_records.by_row result

let _ =
  let* infos = fetch infos_tbl_id in
  let* options = fetch options_tbl_id in
  let* places = fetch places_tbl_id in
  let* task_types = fetch task_types_tbl_id in
  let* time_specs = fetch time_slots_tbl_id in
  let* volunteers = fetch volunteers_tbl_id in
  let* quests = fetch quests_tbl_id in

  let data =
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
  in
  Console.log [ "POUET POUET" ];
  Console.log [ data ];
  let+ res =
    let open Brr_io.Fetch in
    let body = Body.of_jstr (Json.encode data) in

    Console.log [ "BODY"; body ];
    let method' = Jstr.v "PUT" in
    let url = Jstr.v "/grist/data" in
    let headers =
      Headers.of_assoc [ (Jstr.v "Content-Type", Jstr.v "application/json") ]
    in
    let init = Request.init ~body ~method' ~headers () in
    Brr_io.Fetch.url ~init url
  in
  Console.log [ res ]
