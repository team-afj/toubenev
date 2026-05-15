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

  let create ?(responsive = true) ?(maintainAspectRatio = true) () =
    let obj = Jv.obj [||] in
    Jv.Bool.set obj "responsive" responsive;
    Jv.Bool.set obj "maintainAspectRatio" maintainAspectRatio;
    obj

  let set_responsive o b = Jv.Bool.set o "responsive" b
  let set_maintain_aspect_ratio o b = Jv.Bool.set o "maintainAspectRatio" b
  let to_jv t = t
end

(* Chart module *)

module Chart = struct
  type t = Jv.t

  include (Jv.Id : Jv.CONV with type t := t)

  let create ~canvas ~chart_type ~data ~options =
    let ctx =
      Jv.call (El.to_jv canvas) "getContext" [| Jv.of_jstr (Jstr.v "2d") |]
    in
    let config = Jv.obj [||] in
    Jv.Jstr.set config "type" chart_type;
    Jv.set config "data" (Data.to_jv data);
    Jv.set config "options" (Options.to_jv options);
    Jv.new' chart_ctor [| ctx; config |]

  let destroy c = ignore @@ Jv.call c "destroy" [||]
  let update c = ignore @@ Jv.call c "update" [||]

  let get_data c =
    let data_obj = Jv.get c "data" in
    Data.of_jv data_obj

  let to_jv t = t
end
