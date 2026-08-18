open Brr
open Lunar_jsont
open Data_repr
open! Rich
open! Normal

let j = Jstr.v
let c_grid_row = At.class' (j "grid-row")
let c_grid_left = At.class' (j "grid-left")

let grid_template_columns duration =
  Printf.sprintf
    "grid-template-columns: [left] 10rem [timeline] repeat(%i, minmax(0, 1fr));"
    duration
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

let render_ticks ~start ~end_ =
  let bounds_ticks = Zoned_datetime.Set.of_list [ start ] in
  let aligned_start = Zoned_datetime.ceil Lunar.Resolution.hour start in
  let range = Zoned_datetime.Range.make ~first:aligned_start ~last:end_ in
  let ticks =
    Zoned_datetime.(
      Set.add_seq
        (Range.to_seq ~include_boundaries:false ~iterator:Range.iterator_hour
           range)
        bounds_ticks)
  in
  let ticks = Zoned_datetime.Set.remove end_ ticks in
  Zoned_datetime.Set.fold
    (fun dt acc ->
      let start = Zoned_datetime.diff dt start |> Duration.to_minutes in
      let txt = Zoned_datetime.local_time dt |> Time.to_string ~format:`SHORT in
      El.div
        ~at:[ At.class' (j "tick"); grid_column (start + 1) 1 ]
        [ El.div [ El.txt' txt ] ]
      :: acc)
    ticks []
  |> List.rev

let render_background () =
  El.div ~at:[ At.class' (j "grid-background") ] [ El.div [] ]

let render date
    (assignations : Api.assignation list Task_type.Map.t Place.Map.t) =
  let assignations = sort assignations in
  let min_q, max_q = bounds assignations in
  let tts =
    Place.Map.fold
      (fun _p -> Task_type.Map.fold (fun tt _ acc -> Task_type.Set.add tt acc))
      assignations Task_type.Set.empty
  in
  let current_color_deg = ref 0 in
  let color_incr = 360 / Task_type.Set.cardinal tts in
  let tt_colors =
    Task_type.Set.fold tts ~init:Task_type.Map.empty ~f:(fun acc tt ->
        current_color_deg := !current_color_deg + color_incr;
        Task_type.Map.add tt !current_color_deg acc)
  in
  let start = min_q.slot.start in
  let end_ = Time_slot.end_ max_q.slot in
  let minutes = Zoned_datetime.diff end_ start |> Duration.to_minutes in
  let make_slots ?(color = "white") ass =
    List.map ass ~f:(fun { Api.quest; volunteers } ->
        let start_q = quest.slot.start in
        let start = Zoned_datetime.diff start_q start |> Duration.to_minutes in
        let duration = quest.slot.duration |> Duration.to_minutes in
        let short_name_breakpoint = if duration >= 120 then 4 else 2 in
        let fold_volunteers acc =
          if quest.initial.required_volunteers <= short_name_breakpoint then
            fun v -> El.div [ El.txt' v.Volunteer.name ] :: acc
          else fun v ->
            let name =
              String.split_on_char ~sep:' ' v.Volunteer.name |> List.hd
            in
            if List.is_empty acc then [ El.txt' name ]
            else El.txt' name :: El.txt' ", " :: acc
        in
        let sorted_volunteers =
          Volunteers.to_list volunteers
          |> List.sort ~cmp:(fun v v' ->
              let v = String.(trim (uncapitalize_ascii v.Volunteer.name)) in
              let v' = String.(trim (uncapitalize_ascii v'.Volunteer.name)) in
              String.compare v v')
        in
        let names =
          List.fold_left sorted_volunteers ~init:[] ~f:fold_volunteers
        in
        let header =
          let start_time =
            Zoned_datetime.local_time start_q |> Time.to_string ~format:`SHORT
          in
          let end_time =
            Zoned_datetime.local_time (Time_slot.end_ quest.slot)
            |> Time.to_string ~format:`SHORT
          in
          El.txt'
            (start_time ^ " - " ^ end_time ^ ": "
            ^ string_of_int quest.initial.required_volunteers)
        in
        let content =
          El.div [ header ]
          :: List.rev_append names
               [
                 El.div
                   ~at:[ At.class' (j "slot-details") ]
                   [ El.txt' quest.initial.name ];
               ]
        in
        let style = j ("background-color: " ^ color) in
        El.div
          ~at:
            [
              At.class' (j "slot");
              grid_column (start + 1) duration;
              At.style style;
            ]
          [ El.div ~at:[ At.class' (j "slot-content") ] content ])
  in
  let make_row place by_task =
    let color deg = Printf.sprintf "hsl(%ideg 75%% 75%%)" deg in
    let slots =
      List.map by_task ~f:(fun (tt, assignations) ->
          let deg = Task_type.Map.find tt tt_colors in
          let color = color deg in
          (tt, color, make_slots ~color assignations))
    in
    let types =
      List.map slots ~f:(fun (tt, color, _) ->
          let style = j ("background-color: " ^ color) in
          El.div
            ~at:[ At.class' (j "quest-type"); At.style style ]
            [ El.txt' (Task_type.nice_name tt) ])
    in
    let slots = List.flat_map ~f:(fun (_, _, a) -> a) slots in
    El.div ~at:[ c_grid_row ]
      [
        El.div (El.txt' (Place.nice_name place) :: types);
        El.div ~at:[ At.class' (j "grid-timeline") ] slots;
      ]
  in
  let rows =
    Place.Map.fold
      (fun place tts acc ->
        if String.prefix ~pre:"Scène" place.name then
          let tasks = Task_type.Map.to_list tts in
          make_row place tasks :: acc
        else
          Task_type.Map.fold
            (fun task_type assignations acc ->
              make_row place [ (task_type, assignations) ] :: acc)
            tts acc)
      assignations []
  in
  let header =
    let ticks = render_ticks ~start ~end_ in
    El.div ~at:[ c_grid_row ]
      [
        El.div [ El.txt' (Date.to_intl_long_string `Fr date) ];
        El.div ~at:[ At.class' (j "grid-ticks") ] ticks;
      ]
  in
  El.section
    ~at:[ At.class' (Jstr.v "planning-place") ]
    [
      El.div
        ~at:[ At.class' (j "grid-planning"); grid_template_columns minutes ]
        (header :: rows);
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
  Date.Map.fold
    (fun day ass acc ->
      El.section ~at:[ At.class' (Jstr.v "planning-place") ] [ render day ass ]
      :: acc)
    assignations []
  |> List.rev
