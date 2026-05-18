open Brr

(* Color helpers *)

type color = Jstr.t

let rgb r g b = Jstr.v @@ Printf.sprintf "rgb(%d,%d,%d)" r g b
let rgba r g b a = Jstr.v @@ Printf.sprintf "rgba(%d,%d,%d,%.2f)" r g b a

(* Chart.js global reference *)

let chart_ctor = Jv.get Jv.global "Chart"

(* Dataset module *)

module Dataset = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  let create ?label ?border_color ?background_color ?border_width
      ?(border_dash = []) ?tension ?fill ?point_radius ~data () =
    let obj = Jv.obj [||] in
    Jv.Jstr.set_if_some obj "label" label;
    let data_arr = Jv.of_list Jv.of_float data in
    Jv.set obj "data" data_arr;
    Jv.set obj "borderDash" @@ Jv.of_list Jv.of_float border_dash;
    Jv.Jstr.set_if_some obj "borderColor" border_color;
    Jv.Jstr.set_if_some obj "backgroundColor" background_color;
    Jv.Int.set_if_some obj "borderWidth" border_width;
    Jv.Float.set_if_some obj "tension" tension;
    Jv.Bool.set_if_some obj "fill" fill;
    Jv.Int.set_if_some obj "pointRadius" point_radius;
    obj

  let set_data t data =
    let data_arr = Jv.of_list Jv.of_float data in
    Jv.set t "data" data_arr

  let push_data t data =
    let data' = Jv.get t "data" in
    ignore @@ Jv.call data' "push" [| Jv.of_float data |]

  let set_border_color d c = Jv.Jstr.set d "borderColor" c
  let set_background_color d c = Jv.Jstr.set d "backgroundColor" c
  let set_border_width d w = Jv.Int.set d "borderWidth" w
  let set_border_dash t d = Jv.set t "borderDash" (Jv.of_list Jv.of_float d)
  let set_tension d t = Jv.Float.set d "tension" t
  let set_fill d b = Jv.Bool.set d "fill" b
  let set_point_radius d r = Jv.Int.set d "pointRadius" r
  let to_jv t = t
end

(* Data module *)

module Data = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  let create ?labels ?(datasets = []) () =
    let obj = Jv.obj [||] in
    (match labels with
    | Some labels ->
        let labels_arr = Jv.of_list Jv.of_jstr labels in
        Jv.set obj "labels" labels_arr
    | None -> ());
    let datasets_arr = Jv.of_list Dataset.to_jv datasets in
    Jv.set obj "datasets" datasets_arr;
    obj

  let add_dataset d dataset =
    let datasets = Jv.get d "datasets" in
    ignore @@ Jv.call datasets "push" [| Dataset.to_jv dataset |]

  let set_labels t labels =
    let labels_arr = Jv.of_list Jv.of_jstr labels in
    Jv.set t "labels" labels_arr

  let push_label t label =
    let labels = Jv.get t "labels" in
    ignore @@ Jv.call labels "push" [| Jv.of_jstr label |]

  let to_jv t = t
end

(* Options module *)

module Options = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  module Scale = struct
    type t = Jv.t

    include (Jv.Id : Jv.CONV with type t := t)

    let create ?display ?typ ?min ?max ?title ?stacked ?begin_at_zero
        ?grid_display ?ticks_callback ?ticks_color ?ticks_max_ticks_limit
        ?ticks_step_size ?ticks_auto_skip () =
      let obj = Jv.obj [||] in
      Jv.Bool.set_if_some obj "display" display;
      Jv.Jstr.set_if_some obj "type" typ;
      Jv.Float.set_if_some obj "min" min;
      Jv.Float.set_if_some obj "max" max;
      Jv.Bool.set_if_some obj "stacked" stacked;
      Jv.Bool.set_if_some obj "beginAtZero" begin_at_zero;
      let grid = Jv.obj [||] in
      Jv.Bool.set_if_some grid "display" grid_display;
      Jv.set obj "grid" grid;
      let ticks = Jv.obj [||] in
      let cb =
        Option.map
          (fun f -> Jv.callback ~arity:3 (fun v i t -> f v (Jv.to_int i) t))
          ticks_callback
      in
      Jv.set_if_some ticks "callback" cb;
      Jv.Jstr.set_if_some ticks "color" ticks_color;
      Jv.Int.set_if_some ticks "maxTicksLimit" ticks_max_ticks_limit;
      Jv.Float.set_if_some ticks "stepSize" ticks_step_size;
      Jv.Bool.set_if_some ticks "autoSkip" ticks_auto_skip;
      Jv.set obj "ticks" ticks;
      let title_obj = Jv.obj [||] in
      (match title with
      | Some text ->
          Jv.Bool.set title_obj "display" true;
          Jv.Jstr.set title_obj "text" text
      | None -> ());
      Jv.set obj "title" title_obj;
      obj
  end

  module Plugins = struct
    type t = Jv.t

    include (Jv.Id : Jv.CONV with type t := t)

    let create ?legend_display ?legend_position ?legend_align ?legend_reverse
        ?title_display ?title_text ?title_color ?title_position ?title_align
        ?tooltip_enabled ?tooltip_mode ?tooltip_intersect () =
      let obj = Jv.obj [||] in
      let legend = Jv.obj [||] in
      Jv.Bool.set_if_some legend "display" legend_display;
      Jv.Jstr.set_if_some legend "position" legend_position;
      Jv.Jstr.set_if_some legend "align" legend_align;
      Jv.Bool.set_if_some legend "reverse" legend_reverse;
      Jv.set obj "legend" legend;
      let title = Jv.obj [||] in
      Jv.Bool.set_if_some title "display" title_display;
      Jv.Jstr.set_if_some title "text" title_text;
      Jv.Jstr.set_if_some title "color" title_color;
      Jv.Jstr.set_if_some title "position" title_position;
      Jv.Jstr.set_if_some title "align" title_align;
      Jv.set obj "title" title;
      let tooltip = Jv.obj [||] in
      Jv.Bool.set_if_some tooltip "enabled" tooltip_enabled;
      Jv.Jstr.set_if_some tooltip "mode" tooltip_mode;
      Jv.Bool.set_if_some tooltip "intersect" tooltip_intersect;
      Jv.set obj "tooltip" tooltip;
      obj
  end

  module Interaction = struct
    type t = Jv.t

    include (Jv.Id : Jv.CONV with type t := t)

    let create ?mode ?intersect ?axis () =
      let obj = Jv.obj [||] in
      Jv.Jstr.set_if_some obj "mode" mode;
      Jv.Bool.set_if_some obj "intersect" intersect;
      Jv.Jstr.set_if_some obj "axis" axis;
      obj
  end

  let create ?(responsive = true) ?(maintainAspectRatio = true) ?aspect_ratio
      ?resize_delay ?animation ?index_axis ?layout_padding ?(scales = [])
      ?plugins ?interaction () =
    let obj = Jv.obj [||] in
    Jv.Bool.set obj "responsive" responsive;
    Jv.Bool.set obj "maintainAspectRatio" maintainAspectRatio;
    Jv.Float.set_if_some obj "aspectRatio" aspect_ratio;
    Jv.Int.set_if_some obj "resizeDelay" resize_delay;
    Jv.Bool.set_if_some obj "animation" animation;
    Jv.Jstr.set_if_some obj "indexAxis" index_axis;
    (match layout_padding with
    | Some p ->
        let layout = Jv.obj [||] in
        Jv.Int.set layout "padding" p;
        Jv.set obj "layout" layout
    | None -> ());
    (match scales with
    | [] -> ()
    | xs ->
        let scales_obj = Jv.obj [||] in
        Stdlib.List.iter
          (fun (id, scale) ->
            Jv.set scales_obj (Jstr.to_string id) (Scale.to_jv scale))
          xs;
        Jv.set obj "scales" scales_obj);
    Jv.set_if_some obj "plugins" (Option.map Plugins.to_jv plugins);
    Jv.set_if_some obj "interaction" (Option.map Interaction.to_jv interaction);
    obj

  let to_jv t = t
end

(* Chart module *)

module Chart = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  let create ~canvas ~chart_type ?data ~options () =
    let ctx =
      Jv.call (El.to_jv canvas) "getContext" [| Jv.of_jstr (Jstr.v "2d") |]
    in
    let config = Jv.obj [||] in
    Jv.Jstr.set config "type" chart_type;
    Jv.set_if_some config "data" (Option.map Data.to_jv data);
    Jv.set config "options" (Options.to_jv options);
    Jv.new' chart_ctor [| ctx; config |]

  let destroy c = ignore @@ Jv.call c "destroy" [||]
  let update c = ignore @@ Jv.call c "update" [||]

  let get_data c =
    let data_obj = Jv.get c "data" in
    Data.of_jv data_obj

  let set_data t d = Jv.set t "data" @@ Data.to_jv d
  let to_jv t = t
end
