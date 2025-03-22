open Import
open Static_results

type quest = {
  id : string;
  name : string;
  types : quest_type list;
  place : place;
  volunteers : volunteer list;
  start : Calendar.t;
  end_ : Calendar.t; [@key "end"]
}

module Map = Map.Make (String)

type data = {
  quest_types : quest_type Map.t;
  places : place Map.t;
  volunteers : volunteer Map.t;
  quests : quest Map.t;
}

type resource =
  | Quest_type : quest_type -> resource
  | Place : place -> resource
  | Volunteer : volunteer -> resource

let id_of_resource (res : resource) =
  match res with
  | Quest_type { id; _ } -> Printf.sprintf "qt_%s" id
  | Place { id; _ } -> Printf.sprintf "p_%s" id
  | Volunteer { id; _ } -> Printf.sprintf "v_%s" id

let find_resource_by_id data id =
  let open Option in
  let* prefix = String.split_on_char ~by:'_' id |> List.head_opt in
  match prefix with
  | "qt" -> Map.find_opt id data.quest_types >|= fun r -> Quest_type r
  | "p" -> Map.find_opt id data.places >|= fun r -> Place r
  | "v" -> Map.find_opt id data.volunteers >|= fun r -> Volunteer r
  | _ -> None

let load () =
  match Static_results.results with
  | Error _ ->
      {
        quest_types = Map.empty;
        places = Map.empty;
        volunteers = Map.empty;
        quests = Map.empty;
      }
  | Ok { quest_types; places; volunteers; quests } ->
      let quest_types =
        List.fold_left
          ~f:(fun acc ({ id; _ } as elt : quest_type) -> Map.add id elt acc)
          ~init:Map.empty quest_types
      in
      let places =
        List.fold_left
          ~f:(fun acc ({ id; _ } as elt : place) -> Map.add id elt acc)
          ~init:Map.empty places
      in
      let volunteers =
        List.fold_left
          ~f:(fun acc ({ id; _ } as elt : volunteer) -> Map.add id elt acc)
          ~init:Map.empty volunteers
      in
      let quests =
        List.fold_left
          ~f:(fun acc ({ id; name; start; end_; _ } as raw : raw_quest) ->
            let quest =
              {
                id;
                name;
                types =
                  List.filter_map
                    ~f:(fun id -> Map.find_opt id quest_types)
                    raw.types;
                place = Map.find raw.place places;
                volunteers =
                  List.filter_map
                    ~f:(fun id -> Map.find_opt id volunteers)
                    raw.volunteers;
                start = parse_date start;
                end_ = parse_date end_;
              }
            in
            let id = start ^ id in
            Map.add id quest acc)
          ~init:Map.empty quests
      in
      { quest_types; places; volunteers; quests }

open Event_calendar

let to_resources { quest_types; places; volunteers; _ } =
  List.concat
    [
      Map.fold
        (fun _id qt acc ->
          let id = id_of_resource (Quest_type qt) in
          let title = Content.of_string qt.name in
          Plain_resource.make ~id ~title () :: acc)
        quest_types [];
      Map.fold
        (fun _id p acc ->
          let id = id_of_resource (Place p) in
          let title = Content.of_string p.name in
          Plain_resource.make ~id ~title () :: acc)
        places [];
      Map.fold
        (fun _id v acc ->
          let id = id_of_resource (Volunteer v) in
          let title = Content.of_string v.pseudo in
          Plain_resource.make ~id ~title () :: acc)
        volunteers [];
    ]

let quest_resources { types; place; volunteers; _ } =
  List.concat
    [
      List.map ~f:(fun t -> id_of_resource (Quest_type t)) types;
      List.map ~f:(fun v -> id_of_resource (Volunteer v)) volunteers;
      [ id_of_resource (Place place) ];
    ]

let event_extended_props_to_jv q_id = Jv.of_string q_id
let event_extended_props_of_jv jv = Jv.to_string jv

let to_events ?(filter = fun (_ : quest) -> true) t =
  Map.fold
    (fun id ({ start; end_; _ } as q) acc ->
      if not (filter q) then acc
      else
        let start = Js.Date.of_string (print_date start) in
        let end_ = Js.Date.of_string (print_date end_) in
        let resource_ids = quest_resources q in
        let extended_props = event_extended_props_to_jv id in
        Event_calendar.Plain_event.make ~id ~start ~end_ ~resource_ids
          ~extended_props ()
        :: acc)
    t.quests []
