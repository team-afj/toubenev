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
    match !last_data with
    | Some last when Jv.equal last data -> Fut.return (Ok ())
    | _ ->
        let () = last_data := Some data in
        let+ res =
          let open Brr_io.Fetch in
          let body = Body.of_jstr (Json.encode data) in

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
        Console.log [ res ];
        let j = Jv.get Jv.global "JSON" in
        let str =
          Jv.call j "stringify" [| res; Jv.null; Jv.of_int 2 |] |> Jv.to_jstr
        in
        El.set_children elt
        @@ List.map ~f:(fun s -> El.pre [ El.txt s ])
        @@ Jstr.cuts ~sep:(Jstr.v "\\n") str

let _ = Brr.G.set_interval ~ms:2000 sat
