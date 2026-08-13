open Brr
open Lunar_jsont
open Data_repr
open! Rich
open! Normal

let j = Jstr.v

let grid_template_columns duration =
  Printf.sprintf
    "grid-template-columns: [left] 10rem [timeline] repeat(%i, 1fr);" duration
  |> j |> At.style

let grid_column start duration =
  Printf.sprintf "grid-column: %i / span %i" start duration |> j |> At.style

let cmp_quest_time (q : Quest.t) (q' : Quest.t) =
  let c = Zoned_datetime.compare q.slot.start q'.slot.start in
  if c = 0 then
    Zoned_datetime.compare (Time_slot.end_ q.slot) (Time_slot.end_ q'.slot)
  else c

let sort (assignations : Api.assignation list Task_type.Map.t Place.Map.t) =
  let cmp (a : Api.assignation) (b : Api.assignation) =
    cmp_quest_time a.quest b.quest
  in
  Place.Map.map (Task_type.Map.map (List.sort ~cmp)) assignations

let bounds (assignations : Api.assignation list Task_type.Map.t Place.Map.t) =
  let f acc { Api.quest; _ } =
    match acc with
    | None -> Some (quest, quest)
    | Some (f, l) ->
        if cmp_quest_time quest f < 0 then Some (quest, l)
        else if cmp_quest_time quest l > 0 then Some (f, quest)
        else Some (f, l)
  in
  Place.Map.fold
    (fun _ ->
      Task_type.Map.fold (fun _ ass init -> List.fold_left ass ~init ~f))
    assignations None
  |> Option.get_exn_or "Bad bounds"

let render (assignations : Api.assignation list Task_type.Map.t Place.Map.t) =
  let assignations = sort assignations in
  let min_q, max_q = bounds assignations in
  let start = min_q.slot.start in
  let end_ = Time_slot.end_ max_q.slot in
  (* let quest_1 = El.div ~at:[ grid_column 60 60 ] [ El.txt' "60 60" ] in
  let quest_2 = El.div ~at:[ grid_column 120 120 ] [ El.txt' "120 120" ] in *)
  let minutes = Zoned_datetime.diff end_ start |> Duration.to_minutes in
  let ass =
    Place.Map.max_binding assignations
    |> snd |> Task_type.Map.max_binding |> snd
  in
  let shifts =
    List.map ass ~f:(fun { Api.quest; _ } ->
        let start_q = quest.slot.start in
        let start = Zoned_datetime.diff start_q start |> Duration.to_minutes in
        let duration = quest.slot.duration |> Duration.to_minutes in
        El.div
          ~at:[ At.class' (j "slot"); grid_column (start + 1) duration ]
          [
            El.txt' (string_of_int start ^ " " ^ string_of_int duration);
            El.txt' (Zoned_datetime.local_time start_q |> Time.to_string);
          ])
  in
  let q = List.hd ass in
  El.div
    ~at:[ At.class' (j "grid-planning"); grid_template_columns minutes ]
    [
      El.div [ El.txt' q.quest.name ];
      El.div ~at:[ At.class' (j "grid-timeline") ] shifts;
    ]

let render infos (assignations : Api.assignation list) =
  let assignations =
    List.fold_left assignations ~init:Date.Map.empty
      ~f:(fun dates ({ Api.quest; _ } as a) ->
        let date = Normal.to_event_local_date infos quest.slot.start in
        let place = Option.value ~default:Place.dummy quest.initial.place in
        let tt =
          Option.value ~default:Task_type.dummy quest.initial.task_type
        in
        Date.Map.update date
          (function
            | None ->
                Some
                  (Place.Map.singleton place (Task_type.Map.singleton tt [ a ]))
            | Some places ->
                Some
                  (Place.Map.update place
                     (function
                       | None -> Some (Task_type.Map.singleton tt [ a ])
                       | Some tts ->
                           Some
                             (Task_type.Map.update tt
                                (function
                                  | None -> Some [ a ]
                                  | Some ass -> Some (a :: ass))
                                tts))
                     places))
          dates)
  in
  let _, first_day = Date.Map.choose assignations in
  render first_day
