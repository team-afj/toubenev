open Brr

let obj_of_opt_list l = l |> List.filter_map Fun.id |> Array.of_list |> Jv.obj
let opt_field name to_jv = Option.map (fun s -> (name, to_jv s))

type p = Jv.t

module Duration = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"

  let make ?years ?months ?days ?minutes ?seconds () =
    (* todo check not all None *)
    let opt_int_field name = opt_field name Jv.of_int in
    [
      opt_int_field "years" years;
      opt_int_field "months" months;
      opt_int_field "days" days;
      opt_int_field "minutes" minutes;
      opt_int_field "seconds" seconds;
    ]
    |> obj_of_opt_list

  (** a string in the format hh:mm:ss or hh:mm. For example, '05:00' specifies 5
      hours *)
  let of_string s = Jv.of_string s

  (** an integer specifying the total number of seconds *)
  let of_int i = Jv.of_int i
end

module Content = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"

  let of_string s = Jv.of_string s
  let of_html html = Jv.obj [| ("html", Jv.of_string html) |]
  let of_elts elts = Jv.obj [| ("domNodes", Jv.of_list El.to_jv elts) |]
end

module Event = struct
  type t = { id : string; resource_ids : string list; extended_props : Jv.t }

  let of_jv jv =
    let id = Jv.get jv "id" |> Jv.to_string in
    let resource_ids = Jv.get jv "resourceIds" |> Jv.to_list Jv.to_string in
    let extended_props = Jv.get jv "extendedProps" in
    { id; resource_ids; extended_props }

  let to_jv { id; resource_ids; extended_props } =
    Jv.obj
      [|
        ("id", Jv.of_string id);
        ("resource_ids", Jv.of_list Jv.of_string resource_ids);
        ("extendedProps", extended_props);
      |]
end

module Plain_event = struct
  type t = Jv.t

  external of_jv : Jv.t -> t = "%identity"
  external to_jv : t -> Jv.t = "%identity"

  let make ?id ~start ~end_ ?title ?resource_ids ?extended_props () =
    let id = opt_field "id" Jv.of_string id in
    let start = Some ("start", Js.Date.to_jv start) in
    let end_ = Some ("end", Js.Date.to_jv end_) in
    let title = opt_field "title" Content.to_jv title in
    let resource_ids =
      opt_field "resourceIds" (Jv.of_list Jv.of_string) resource_ids
    in
    let extended_props = opt_field "extendedProps" Fun.id extended_props in
    [ id; start; end_; title; resource_ids; extended_props ] |> obj_of_opt_list
end

module Plain_resource = struct
  type t = Jv.t

  external of_jv : Jv.t -> t = "%identity"
  external to_jv : t -> Jv.t = "%identity"

  let make ~id ?title ?event_background_color ?event_text_color ?children () =
    let id = Some ("id", Jv.of_string id) in
    let title = opt_field "title" Content.to_jv title in
    let event_background_color =
      opt_field "eventBackgroundColor" Jv.of_string event_background_color
    in
    let event_text_color =
      opt_field "eventTextColor" Jv.of_string event_text_color
    in
    let children = opt_field "children" Jv.of_jv_list children in
    [ id; title; event_background_color; event_text_color; children ]
    |> obj_of_opt_list
end

module Info = struct
  type t = Jv.t

  external of_jv : Jv.t -> t = "%identity"
  external to_jv : t -> Jv.t = "%identity"

  let event t = Jv.get t "event" |> Event.of_jv
  let time_text t = Jv.get t "timeText" |> Jv.to_string
  let view t = Jv.get t "view" (* todo *)
end

external get_calendar : unit -> Jv.t = "get_calendar"
external get_day_grid : unit -> p = "get_day_grid"
external get_time_grid : unit -> p = "get_time_grid"
external get_list : unit -> p = "get_list"
external get_resource_timeline : unit -> p = "get_resource_timeline"
external get_resource_timegrid : unit -> p = "get_resource_timegrid"

type plugin = DayGrid | List | ResourceTimeline | ResourceTimeGrid | TimeGrid

let load_plugin : plugin -> p = function
  | DayGrid -> get_day_grid ()
  | List -> get_list ()
  | ResourceTimeline -> get_resource_timeline ()
  | ResourceTimeGrid -> get_resource_timegrid ()
  | TimeGrid -> get_time_grid ()

type view = Time_grid_week | Resource_timeline_day | Resource_timegrid_week

let jv_of_view = function
  | Time_grid_week -> Jv.of_string "timeGridWeek"
  | Resource_timeline_day -> Jv.of_string "resourceTimelineDay"
  | Resource_timegrid_week -> Jv.of_string "resourceTimeGridWeek"

type t = Jv.t
type header_toolbar = Jv.t

let header_toolbar ?(start = "") ?(center = "") ?(end_ = "") () =
  let start = ("start", Jv.of_string start) in
  let center = ("center", Jv.of_string center) in
  let end_ = ("end", Jv.of_string end_) in
  Jv.obj [| start; center; end_ |]

let make ~target ?(plugins = []) ?view ?date ?duration ?scroll_time
    ?filter_events_with_resources ?filter_resources_with_events ?now_indicator
    ?editable ?event_start_editable ?event_duration_editable ?event_content
    ?header_toolbar () : t =
  let target = El.to_jv target in
  let options =
    let view = opt_field "view" jv_of_view view in
    let date = opt_field "date" Js.Date.to_jv date in
    let duration = opt_field "duration" Duration.to_jv duration in
    let scroll_time = opt_field "scrollTime" Duration.to_jv scroll_time in
    let filter_events_with_resources =
      opt_field "filterEventsWithResources" Jv.of_bool
        filter_events_with_resources
    in
    let filter_resources_with_events =
      opt_field "filterResourcesWithEvents" Jv.of_bool
        filter_resources_with_events
    in
    let now_indicator = opt_field "nowIndicator" Jv.of_bool now_indicator in
    let editable = opt_field "editable" Jv.of_bool editable in
    let event_start_editable =
      opt_field "eventStartEditable" Jv.of_bool event_start_editable
    in
    let event_duration_editable =
      opt_field "eventDurationEditable" Jv.of_bool event_duration_editable
    in
    let event_content =
      Option.map
        (fun f ->
          (* let f_jv info = f (Info.of_jv info) in *)
          ("eventContent", Jv.callback ~arity:1 f))
        event_content
    in
    let header_toolbar = opt_field "headerToolbar" Fun.id header_toolbar in
    [
      view;
      date;
      duration;
      scroll_time;
      filter_events_with_resources;
      filter_resources_with_events;
      now_indicator;
      editable;
      event_start_editable;
      event_duration_editable;
      event_content;
      header_toolbar;
    ]
    |> obj_of_opt_list
  in
  let plugins = List.map load_plugin plugins |> Jv.of_jv_list in
  let props = Jv.obj [| ("plugins", plugins); ("options", options) |] in
  let params = Jv.obj [| ("target", target); ("props", props) |] in
  Console.log [ get_calendar (); params ];
  Jv.new' (get_calendar ()) [| params |]

type 'a option =
  | Events : Plain_event.t list option
  | Resources : Plain_resource.t list option

let set_option (type a) t (opt : a option) (value : a) =
  let name, value =
    match opt with
    | Events -> ("events", Jv.of_list Plain_event.to_jv value)
    | Resources -> ("resources", Jv.of_list Plain_resource.to_jv value)
  in
  Jv.call t "setOption" [| Jv.of_string name; value |] |> ignore
