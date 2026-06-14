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

let make_assignation_cell ?tdq el =
  match tdq with
  | None -> El.td [ el ]
  | Some tdq ->
      El.td [ El.span ~at:[ cls "planning-cell-tdq" ] [ El.txt' tdq ]; el ]

let make_empty_cell ?tdq () = make_assignation_cell ?tdq (El.nbsp ())
let make_assigned_cell ?tdq name = make_assignation_cell ?tdq (El.txt' name)

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
           List.fold_left assignations ~init:acc
             ~f:(fun acc { Api.quest; volunteers } ->
               let slot = Normal.Time_slot.to_string quest.slot in
               let n = max 1 quest.initial.required_volunteers in
               let n_missing =
                 max 0 (n - Normal.Volunteers.cardinal volunteers - 1)
               in
               let tdq =
                 match (with_types, quest.initial.task_type) with
                 | true, Some tdq -> Some tdq.slug
                 | _, _ -> None
               in
               let first, rest =
                 match Normal.Volunteers.to_list volunteers with
                 | [] -> (make_empty_cell ?tdq (), [])
                 | hd :: tl -> (make_assigned_cell ?tdq hd.name, tl)
               in
               let missing =
                 List.init ~len:n_missing ~f:(fun _ ->
                     El.tr [ make_empty_cell ?tdq () ])
               in
               let rest =
                 List.fold_left rest ~init:(List.rev_append missing acc)
                   ~f:(fun acc (v : Normal.Volunteer.t) ->
                     El.tr [ make_assigned_cell ?tdq v.name ] :: acc)
               in
               El.tr [ El.th ~at:[ rowspan n ] [ El.txt' slot ]; first ] :: rest))
         []
  in
  El.div [ El.table ~at:[ cls "planning" ] (head :: rows) ]

let make_day_table ~with_types acc (d, a) =
  let el = make_day_table ~with_types d a in
  el :: acc

let make_task_type_legend types =
  let lis =
    Task_type.Set.fold types ~init:[] ~f:(fun acc tdq ->
        let txt =
          match tdq.slug with "" -> tdq.name | slug -> slug ^ " " ^ tdq.name
        in
        El.span [ El.txt' txt ] :: acc)
  in
  Pico_ui.El.section [ El.div ~at:[ cls "planning-legend" ] lis ]

let make_place_planning (place : Place.t) assignations =
  let title = El.h1 [ El.txt' ("Planning " ^ place.name) ] in
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
  let legend = make_task_type_legend types in
  let days = El.div ~at:[ cls "day-grid" ] days in
  El.section ~at:[ At.class' (Jstr.v "planning-place") ] [ title; legend; days ]

let make_plannings (data : Rich.Planning.t) (answer : Api.answer) =
  let assignations =
    List.fold_left answer.solution ~init:Place.Map.empty
      ~f:(fun acc ({ Api.quest; _ } as ass) ->
        let place = Option.value ~default:Place.dummy quest.initial.place in
        let date = Normal.to_event_local_date data.infos quest.slot.start in
        let time = quest.slot.start in
        (* let end_time = Normal.Time_slot.end_ quest.slot in *)
        Place.Map.update place
          (function
            | None ->
                Some
                  (Date.Map.singleton date
                     (Zoned_datetime.Map.singleton time [ ass ]))
            | Some dates ->
                Some
                  (Date.Map.update date
                     (function
                       | None ->
                           Some (Zoned_datetime.Map.singleton time [ ass ])
                       | Some times ->
                           Some
                             (Zoned_datetime.Map.update time
                                (function
                                  | None -> Some [ ass ]
                                  | Some assignations ->
                                      Some (ass :: assignations))
                                times))
                     dates))
          acc)
  in
  let make_place_planning p v acc = make_place_planning p v :: acc in
  let place_sections = Place.Map.fold make_place_planning assignations [] in
  El.section
    ~at:[ cls "planning-view" ]
    [ El.div ~at:[ cls "planning-sections" ] place_sections ]
