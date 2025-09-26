open Import
open! Brr
open Brr_lwd

let listen ?(initial_trigger = false) ~f t =
  let root = Lwd.observe t in
  Lwd.set_on_invalidate root (fun _ -> f (Lwd.quick_sample root));
  let first_sample = Lwd.quick_sample root in
  if initial_trigger then f first_sample

let data = Data.load ()

let filters_el =
  El.find_first_by_selector (Jstr.v "#filters") |> Option.get_exn_or ""

let calendar_el =
  El.find_first_by_selector (Jstr.v "#calendar") |> Option.get_exn_or ""

let date, scroll_time =
  let _, first_quest = Data.Map.min_binding data.quests in
  let minutes = (first_quest.start |> Calendar.hour) * 60 in
  let scroll_time = Event_calendar.Duration.make ~minutes () in
  (Js.Date.of_string (print_date first_quest.start), scroll_time)

let duration = Event_calendar.Duration.make ~days:3 ()
let () = Console.log [ "Date"; date; "Duration"; duration ]
(* let active_volunteer = Lwd.var None *)

let active_volunteer_select =
  Brr_lwd_ui.Forms.Field_select.make ~persist:false
    { name = "pouet"; default = "all"; label = [] }
    (Lwd.pure
    @@ Lwd_seq.of_list
         (("all", "Toustes")
         :: (Data.Map.to_list data.volunteers
            |> List.map ~f:(fun (id, v) -> (id, v.Static_results.pseudo)))))

let select_categories =
  let open Brr_lwd_ui.Forms in
  let options =
    let open Field_checkboxes in
    let types =
      Data.Map.to_list data.quest_types
      |> List.map ~f:(fun (id, ({ name; _ } : Data.quest_type)) ->
             Check
               {
                 value = `Quest_type id;
                 id = name;
                 name;
                 label = (fun () -> [ `P (El.txt' name) ]);
                 state = true;
               })
    in
    let places =
      Data.Map.to_list data.places
      |> List.map ~f:(fun (id, ({ name; _ } : Data.place)) ->
             Check
               {
                 value = `Place id;
                 id = name;
                 name;
                 label = (fun () -> [ `P (El.txt' name) ]);
                 state = false;
               })
    in
    let volunteers =
      Data.Map.to_list data.volunteers
      |> List.map ~f:(fun (id, ({ pseudo; _ } : Data.volunteer)) ->
             Check
               {
                 value = `Volunteer id;
                 id = pseudo;
                 name = pseudo;
                 label = (fun () -> [ `P (El.txt' pseudo) ]);
                 state = false;
               })
    in
    let flat = List.concat [ types; places; volunteers ] in
    { name = "grp1"; desc = Lwd.pure (Lwd_seq.of_list flat) }
  in
  Field_select.make_multiple
    ~at:[ `P (At.class' (Jstr.v "categories_select")) ]
    options

let filters =
  Elwd.div [ `R active_volunteer_select.field; `R select_categories.field ]

let active_volunteer = active_volunteer_select.value

let event_content (info : Event_calendar.Info.t) =
  let event = Event_calendar.Info.event info in
  let quest = Data.Map.find event.id data.quests in
  let volunteers_link acc (v : Static_results.volunteer) =
    let el = El.a ~at:[ At.href (Jstr.v "") ] [ El.txt' v.pseudo ] in
    let _ =
      Ev.listen Ev.click
        (fun ev ->
          Ev.prevent_default ev;
          Lwd.set active_volunteer v.id)
        (El.as_target el)
    in
    if List.is_empty acc then [ el ]
    else List.rev_append [ El.txt' ", "; el ] acc
  in
  let volunteers =
    List.fold_left ~f:volunteers_link ~init:[] quest.volunteers
  in
  let date = Event_calendar.Info.time_text info in
  let text = volunteers in
  let icon =
    (List.hd quest.types).name |> String.split_on_char ~by:' ' |> List.hd
  in
  Event_calendar.Content.of_elts
    [
      El.h4
        [
          El.txt' (date ^ " " ^ icon ^ " " ^ String.capitalize_ascii quest.name);
        ];
      (* El.p [ El.txt' () ]; *)
      El.p text;
    ]

let c =
  let header_toolbar =
    Event_calendar.header_toolbar ~start:"title"
      ~center:"resourceTimelineWeek,resourceTimeGridWeek,listDay,timeGridWeek"
      ~end_:"today prev,next" ()
  in
  let slot_min_time = Event_calendar.Duration.make ~minutes:(5 * 60) () in
  let slot_max_time =
    Event_calendar.Duration.make ~minutes:((24 + 5) * 60) ()
  in
  Event_calendar.make ~target:calendar_el
    ~plugins:[ DayGrid; List; ResourceTimeGrid; ResourceTimeline; TimeGrid ]
    ~date ~duration ~scroll_time ~event_content
    ~filter_events_with_resources:true ~filter_resources_with_events:true
    ~now_indicator:true ~header_toolbar ~slot_max_time ~slot_min_time ()

let () = Event_calendar.set_option c Resources (Data.to_resources data)

let update_events ~filter =
  let events = Data.to_events ?filter data in
  Event_calendar.set_option c Events events

let update_resources data =
  let events = Data.to_resources data in
  Event_calendar.set_option c Resources events

(* History management *)
let history = Window.history G.window

(* Set initial state according to uri params *)
let () =
  let params = Window.location G.window |> Uri.fragment_params in
  Option.iter (fun user -> Lwd.set active_volunteer (Jstr.to_string user))
  @@ Uri.Params.find (Jstr.v "user") params

let update_fragment v =
  let open Window.History in
  let uri = Window.location G.window in
  let uri =
    Uri.with_fragment_params uri
    @@ Uri.Params.of_assoc [ (Jstr.v "user", Jstr.v v) ]
  in
  replace_state ~uri history

let () =
  let on_state_change v =
    Console.log [ "Active: "; v ];
    let filter =
      match v with
      | "all" -> None
      | id ->
          Some
            (fun (q : Data.quest) ->
              List.exists
                ~f:(fun (v : Static_results.volunteer) -> String.equal v.id id)
                q.volunteers)
    in
    update_fragment v;
    update_events ~filter
  in
  listen ~initial_trigger:true ~f:on_state_change @@ Lwd.get active_volunteer

let () =
  let f v =
    let ids = Lwd_seq.to_list v |> List.map ~f:(fun (v, _) -> v) in
    let quest_types, places, volunteers =
      List.fold_left
        ~f:(fun (qt_acc, p_acc, v_acc) -> function
          | `Quest_type qtid ->
              let open Data in
              ( Map.add qtid (Map.find qtid data.quest_types) qt_acc,
                p_acc,
                v_acc )
          | `Place id ->
              let open Data in
              (qt_acc, Map.add id (Map.find id data.places) p_acc, v_acc)
          | `Volunteer id ->
              let open Data in
              (qt_acc, p_acc, Map.add id (Map.find id data.volunteers) v_acc))
        ~init:(Data.Map.empty, Data.Map.empty, Data.Map.empty)
        ids
    in
    (*
    update_fragment v; *)
    update_resources { data with quest_types; places; volunteers }
  in
  listen ~initial_trigger:true ~f select_categories.value

let app = Elwd.div [ `R filters ]

let _filters_ui =
  let on_load _ =
    let app = Lwd.observe @@ app in
    let on_invalidate _ =
      ignore @@ G.request_animation_frame
      @@ fun _ -> ignore @@ Lwd.quick_sample app
    in
    El.append_children filters_el [ Lwd.quick_sample app ];
    Lwd.set_on_invalidate app on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)
