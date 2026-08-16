module B64 = Base64
open Brr
open Fut.Result_syntax
open! Data_repr

let debug = false

let maybe_decompress jstr =
  (* Required for compatibility pre-compression *)
  if not (Jstr.is_empty jstr || Jstr.starts_with ~prefix:(Jstr.v "{\"") jstr)
  then
    Jstr.to_string jstr |> B64.decode_exn |> Decompress.decompress_string
    |> Result.map_error (fun (`Msg msg) -> msg)
    |> Result.get_or_failwith |> Option.some
  else None

let maybe_decompress_f jstr =
  match maybe_decompress jstr with
  | Some str -> str
  | None -> Jstr.to_string jstr

module Data = struct
  let infos_tbl_id = Jstr.v "Infos_generales"
  let options_tbl_id = Jstr.v "Options_du_solveur"
  let spread_prefs_tbl_id = Jstr.v "Spread_taste"
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
    let* spread_prefs = fetch spread_prefs_tbl_id in
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
          ("spread_prefs", spread_prefs);
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

  let id jv = Jv.Int.get jv "id"
  let name jv = Jv.Jstr.get jv "name"

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

  let create_solution ~name state =
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

  let upsert_solution id state =
    let json =
      Jsont_bytesrw.encode_string jsont state |> Result.get_or_failwith
    in
    let l = String.length json in
    let json = Decompress.compress_string ~level:8 json |> B64.encode_exn in
    let l' = String.length json in
    Console.error [ "Compress "; l; " -> "; l' ];
    let records =
      [
        Grist.Record.v ~id
          ~fields:[| (Jstr.v "last_answer", Jv.of_string json) |]
          ();
      ]
    in
    Grist.Table_operations.update (table ()) ~records ()

  let get_solution i =
    let+ solutions = Data.fetch Data.solutions_tbl_id in
    let solutions = Jv.to_jv_list solutions in
    let first = List.find solutions ~f:(fun jv -> Jv.Int.get jv "id" = i) in
    let () =
      match maybe_decompress (Jv.Jstr.get first "last_answer") with
      | Some json -> Jstr.v json |> Jv.Jstr.set first "last_answer"
      | None -> ()
    in
    first

  let get_solution_1 () =
    let* s = get_solution 1 in
    let answer = Jv.get s "last_answer" in
    Fut.return @@ Jsont_brr.decode jsont (Jv.to_jstr answer)
end

module Assignations = struct
  open Data_repr.Normal

  let assignations_table () =
    Lazy.force (lazy (Grist.get_table ~table_id:Data.assignations_tbl_id ()))

  let get_assignations ~solution =
    let+ current_assignations = Data.(fetch assignations_tbl_id) in
    let current_assignations = Jv.to_jv_list current_assignations in
    List.filter
      ~f:(fun obj -> Jv.Int.get obj "solution" = solution)
      current_assignations

  let resolve_assignation_jv (data : Api.data) ass =
    let quest_id = Jv.Jstr.get ass "ref" |> Jstr.to_string in
    let volunteers_ids =
      let jv = Jv.get ass "volunteers" in
      (* TODO when there is only one volunteer, it's a number.
          When there are several the list starts with "L" in grist *)
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
          with _err ->
            Console.error [ "TBN ASS OUPS "; "Did not found volunteer #"; i ];
            acc)
    in
    { Api.quest; volunteers }

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
