open Brr
open Lunar_jsont
open Data_repr
open Rich

let text = El.txt'
let cls name = At.class' (Jstr.v name)
let colspan i = At.v (Jstr.v "colspan") (Jstr.of_int i)
let rowspan i = At.v (Jstr.v "rowspan") (Jstr.of_int i)
let scope s = At.v (Jstr.v "scope") (Jstr.v s)

let event_day (infos : Event_infos.t) (slot : Normal.Time_slot.t) =
  let offseted =
    Zoned_datetime.(slot.start - Time.to_duration infos.day_start_utc)
  in
  Zoned_datetime.local_date offseted

let place_of_assignation (assignation : Api.assignation) =
  assignation.quest.initial.Rich.Quest.place

let make_day_table (date : Date.t) assignations =
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
         (fun acc (_datetime, { Api.quest; volunteers }) ->
           let slot = Normal.Time_slot.to_string quest.slot in
           let n = Normal.Volunteers.cardinal volunteers in
           let first, rest =
             match Normal.Volunteers.to_list volunteers with
             | [] -> (El.nbsp (), [])
             | hd :: tl -> (El.txt' hd.name, tl)
           in
           let rest =
             List.fold_left rest ~init:acc
               ~f:(fun acc (v : Normal.Volunteer.t) ->
                 El.tr [ El.td [ El.txt' v.name ] ] :: acc)
           in
           El.tr [ El.th ~at:[ rowspan n ] [ El.txt' slot ]; El.td [ first ] ]
           :: rest)
         []
  in
  El.div [ El.table ~at:[ cls "planning" ] (head :: rows) ]

let make_day_table acc (d, a) = make_day_table d a :: acc

let make_place_planning (place : Place.t) assignations =
  let title = El.h1 [ El.txt' ("Planning " ^ place.name) ] in
  let days = Date.Map.to_rev_seq assignations |> Seq.fold make_day_table [] in
  let days = El.div ~at:[ cls "day-grid" ] days in
  El.section ~at:[ At.class' (Jstr.v "planning-place") ] [ title; days ]

let make_plannings (data : Rich.Planning.t) (answer : Api.answer) =
  let assignations =
    List.fold_left answer.solution ~init:Place.Map.empty
      ~f:(fun acc ({ Api.quest; _ } as ass) ->
        let place = Option.value ~default:Place.dummy quest.initial.place in
        let date = event_day data.infos quest.slot in
        let time = quest.slot.start in
        Place.Map.update place
          (function
            | None ->
                Some
                  (Date.Map.singleton date
                     (Zoned_datetime.Map.singleton time ass))
            | Some dates ->
                Some
                  (Date.Map.update date
                     (function
                       | None -> Some (Zoned_datetime.Map.singleton time ass)
                       | Some times ->
                           Some (Zoned_datetime.Map.add time ass times))
                     dates))
          acc)
  in
  let make_place_planning p v acc = make_place_planning p v :: acc in
  let place_sections = Place.Map.fold make_place_planning assignations [] in
  El.section
    ~at:[ cls "planning-view" ]
    [ El.div ~at:[ cls "planning-sections" ] place_sections ]
