open Brr
open Lunar_jsont
open Data_repr
open Rich

let text = El.txt'
let cls name = At.class' (Jstr.v name)
let colspan i = At.v (Jstr.v "colspan") (Jstr.of_int i)
let rowspan i = At.v (Jstr.v "rowspan") (Jstr.of_int i)
let scope s = At.v (Jstr.v "scope") (Jstr.v s)

let place_of_assignation (assignation : Api.assignation) =
  assignation.quest.initial.Rich.Quest.place

let make_assignation_cell ?tags el =
  match tags with
  | None -> El.td [ el ]
  | Some tags ->
      El.td [ El.span ~at:[ cls "planning-cell-tags" ] [ El.txt' tags ]; el ]

let make_empty_cell ?tags () = make_assignation_cell ?tags (El.nbsp ())
let make_assigned_cell ?tags name = make_assignation_cell ?tags (El.txt' name)

let make_day_table ~with_types (date : Date.t) assignations =
  let head =
    let title =
      El.tr
        [
          El.th
            ~at:[ colspan 2 ]
            [ El.txt' (Date.to_intl_long_string `Fr date) ];
        ]
    in
    El.thead
      [
        title;
        El.tr [ El.th [ El.txt' "Horaire" ]; El.th [ El.txt' "Bénévole" ] ];
      ]
  in
  let rows =
    Zoned_datetime.Map.to_rev_seq assignations
    |> Seq.fold
         (fun acc (_datetime, assignations) ->
           let assignations =
             List.sort
               ~cmp:(fun { Api.quest = q; _ } { Api.quest = q'; _ } ->
                 Zoned_datetime.compare
                   (Normal.Time_slot.end_ q'.slot)
                   (Normal.Time_slot.end_ q.slot))
               assignations
           in
           List.fold_left assignations ~init:acc
             ~f:(fun acc { Api.quest; volunteers } ->
               let slot = Normal.Time_slot.to_string quest.slot in
               let n = max 1 quest.initial.required_volunteers in
               let n_missing =
                 max 0 (n - Normal.Volunteers.cardinal volunteers - 1)
               in
               let tags =
                 match (with_types, quest.initial.task_type) with
                 | true, Some tdq -> Some tdq.slug
                 | _, _ -> None
               in
               let first, rest =
                 match Normal.Volunteers.to_list volunteers with
                 | [] -> (make_empty_cell ?tags (), [])
                 | hd :: tl -> (make_assigned_cell ?tags hd.name, tl)
               in
               let missing =
                 List.init ~len:n_missing ~f:(fun _ ->
                     El.tr [ make_empty_cell ?tags () ])
               in
               let rest =
                 List.fold_left rest ~init:(List.rev_append missing acc)
                   ~f:(fun acc (v : Normal.Volunteer.t) ->
                     El.tr [ make_assigned_cell ?tags v.name ] :: acc)
               in
               El.tr [ El.th ~at:[ rowspan n ] [ El.txt' slot ]; first ] :: rest))
         []
  in
  El.div [ El.table ~at:[ cls "planning" ] (head :: rows) ]

let make_day_table ~with_types acc (d, a) =
  let el = make_day_table ~with_types d a in
  el :: acc

let make_legend v =
  let lis =
    List.fold_left v ~init:[] ~f:(fun acc (slug, name) ->
        let txt = match slug with "" -> name | slug -> slug ^ " " ^ name in
        El.span [ El.txt' txt ] :: acc)
  in
  Pico_ui.El.section [ El.div ~at:[ cls "planning-legend" ] lis ]

let make_layout ~title ~legend content =
  let title = El.h1 [ El.txt' ("Planning " ^ title) ] in
  El.section
    ~at:[ At.class' (Jstr.v "planning-place") ]
    [ title; legend; content ]

let make_place_planning (place : Place.t) assignations =
  let types =
    Date.Map.fold
      (fun _date v acc ->
        Zoned_datetime.Map.fold
          (fun _zdate assignations acc ->
            List.fold_left ~init:acc
              ~f:(fun acc assignation ->
                match assignation.Api.quest.initial.task_type with
                | None -> acc
                | Some tdq -> Task_type.Set.add tdq acc)
              assignations)
          v acc)
      assignations Task_type.Set.empty
  in
  let days =
    Date.Map.to_rev_seq assignations
    |> Seq.fold
         (make_day_table ~with_types:(Task_type.Set.cardinal types > 1))
         []
  in
  let legend =
    types
    |> Task_type.Set.to_list_map ~f:(fun { Task_type.slug; name; _ } ->
        (slug, name))
    |> make_legend
  in
  let content = El.div ~at:[ cls "day-grid" ] days in
  make_layout ~title:place.name ~legend content

type grouping = By_place | By_quest_kind

let group (infos : Event_infos.t) (assignations : Api.assignation list) ~empty
    ~update =
  List.fold_left assignations ~init:empty
    ~f:(fun acc ({ Api.quest; _ } as ass) ->
      let date = Normal.to_event_local_date infos quest.slot.start in
      let time = quest.slot.start in
      (* let end_time = Normal.Time_slot.end_ quest.slot in *)
      update quest
        (function
          | None ->
              Some
                (Date.Map.singleton date
                   (Zoned_datetime.Map.singleton time [ ass ]))
          | Some dates ->
              Some
                (Date.Map.update date
                   (function
                     | None -> Some (Zoned_datetime.Map.singleton time [ ass ])
                     | Some times ->
                         Some
                           (Zoned_datetime.Map.update time
                              (function
                                | None -> Some [ ass ]
                                | Some assignations -> Some (ass :: assignations))
                              times))
                   dates))
        acc)

let group_by_place (infos : Event_infos.t) (assignations : Api.assignation list)
    =
  let update (q : Normal.Quest.t) f acc =
    let place = Option.value ~default:Place.dummy q.initial.place in
    Place.Map.update place f acc
  in
  group infos assignations ~empty:Place.Map.empty ~update

let make_plannings (data : Rich.Planning.t) (answer : Api.answer) =
  let assignations = group_by_place data.infos answer.solution in
  let make_place_planning p v acc = make_place_planning p v :: acc in
  let place_sections = Place.Map.fold make_place_planning assignations [] in
  El.section
    ~at:[ cls "planning-view" ]
    [ El.div ~at:[ cls "planning-sections" ] place_sections ]
