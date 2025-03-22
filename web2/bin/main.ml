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

let date =
  let _, first_quest = Data.Map.min_binding data.quests in
  Js.Date.of_string (print_date first_quest.start)

(* let active_volunteer = Lwd.var None *)

let active_volunteer_select =
  Brr_lwd_ui.Forms.Field_select.make ~persist:false
    { name = "pouet"; default = "all"; label = [] }
    (Lwd.pure
    @@ Lwd_seq.of_list
         (("all", "Toustes")
         :: (Data.Map.to_list data.volunteers
            |> List.map ~f:(fun (id, v) -> (id, v.Static_results.pseudo)))))

let filters = active_volunteer_select.field
let active_volunteer = active_volunteer_select.value

let event_content (info : Event_calendar.Info.t) =
  let event = Event_calendar.Info.event info in
  let quest = Data.Map.find event.id data.quests in
  let volunteers_link acc (v : Static_results.volunteer) =
    let el = El.a ~at:[ At.href (Jstr.v "#") ] [ El.txt' v.pseudo ] in
    let _ =
      Ev.listen Ev.click
        (fun _ -> Lwd.set active_volunteer v.id)
        (El.as_target el)
    in
    if List.is_empty acc then [ el ]
    else List.rev_append [ El.txt' ", "; el ] acc
  in
  let volunteers =
    List.fold_left ~f:volunteers_link ~init:[] quest.volunteers
  in
  let text = volunteers @ [ El.txt' ": "; El.txt' quest.name ] in
  Event_calendar.Content.of_elts [ El.p text ]

let c =
  Event_calendar.make ~target:calendar_el ~plugins:[ List; ResourceTimeline ]
    ~date ~event_content ~filter_events_with_resources:true
    ~filter_resources_with_events:true ()

let () = Event_calendar.set_option c Resources (Data.to_resources data)

let update_events ~filter =
  let events = Data.to_events ?filter data in
  Event_calendar.set_option c Events events

let () =
  listen ~initial_trigger:true ~f:(fun v ->
      Console.log [ "Active: "; v ];
      let filter =
        match v with
        | "all" -> None
        | id ->
            Some
              (fun (q : Data.quest) ->
                List.exists
                  ~f:(fun (v : Static_results.volunteer) ->
                    String.equal v.id id)
                  q.volunteers)
      in
      update_events ~filter)
  @@ Lwd.get active_volunteer

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
