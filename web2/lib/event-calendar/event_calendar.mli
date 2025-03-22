module Duration : sig
  type t

  val to_jv : t -> Jv.t

  val make :
    ?years:int ->
    ?months:int ->
    ?days:int ->
    ?minutes:int ->
    ?seconds:int ->
    unit ->
    t

  val of_string : string -> t
  val of_int : int -> t
end

module Content : sig
  type t

  val to_jv : t -> Jv.t
  val of_string : string -> t
  val of_html : string -> t
  val of_elts : Brr.El.t list -> t
end

module Event : sig
  type t = { id : string; resource_ids : string list; extended_props : Jv.t }

  val of_jv : Jv.t -> t
  val to_jv : t -> Jv.t
end

module Plain_event : sig
  type t

  val of_jv : Jv.t -> t
  val to_jv : t -> Jv.t

  val make :
    ?id:string ->
    start:Js.Date.t ->
    end_:Js.Date.t ->
    ?title:Content.t ->
    ?resource_ids:string list ->
    ?extended_props:Jv.t ->
    unit ->
    t
end

module Plain_resource : sig
  type t

  val of_jv : Jv.t -> t
  val to_jv : t -> Jv.t

  val make :
    id:string ->
    ?title:Content.t ->
    ?event_background_color:string ->
    ?event_text_color:string ->
    ?children:t list ->
    unit ->
    t
end

module Info : sig
  type t

  external of_jv : Jv.t -> t = "%identity"
  external to_jv : t -> Jv.t = "%identity"
  val event : t -> Event.t
  val time_text : t -> string
  val view : t -> Jv.t
end

type plugin = List | ResourceTimeline
type view = Time_grid_week | Resource_timeline_day
type t

val make :
  target:Brr.El.t ->
  ?plugins:plugin list ->
  ?view:view ->
  ?date:Js.Date.t ->
  ?duration:Duration.t ->
  ?filter_events_with_resources:bool ->
  ?filter_resources_with_events:bool ->
  ?editable:bool ->
  ?event_start_editable:bool ->
  ?event_duration_editable:bool ->
  ?event_content:(Info.t -> Content.t) ->
  unit ->
  t

type 'a option =
  | Events : Plain_event.t list option
  | Resources : Plain_resource.t list option

val set_option : t -> 'a option -> 'a -> unit
