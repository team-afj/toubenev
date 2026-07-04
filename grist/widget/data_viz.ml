open Brrer.Brr
open! Brr_lwd
module Color = El.Style.Color

module Linear_gradient = struct
  let make ~low_color ~legend ~high_color data =
    let color =
      let grad = Color.mark_mix low_color high_color in
      fun ratio -> Color.to_css (grad ratio)
    in
    let colors, labels, count =
      List.fold_left
        ~f:(fun (acc_colors, acc_labels, count) (ratio, label) ->
          (color ratio :: acc_colors, label :: acc_labels, count + 1))
        ~init:([], [], 0) data
    in
    let colors, labels = (List.rev colors, List.rev labels) in
    let labels =
      let style =
        Jstr.v {css| flex: 1 1 auto; border-bottom: none; cursor: crosshair|css}
      in
      let tooltip placement txt =
        [
          At.v (Jstr.v "data-tooltip") (Jstr.v txt);
          At.v (Jstr.v "data-placement") (Jstr.v placement);
        ]
      in
      List.mapi labels ~f:(fun i label ->
          let placement =
            if i < count / 4 then "right"
            else if i < 3 * (count / 4) then "bottom"
            else "left"
          in
          El.div ~at:(At.style style :: tooltip placement label) [])
    in
    let gradient =
      let ccs_gradient =
        "background: linear-gradient(to right, "
        ^ String.concat ~sep:", " colors
        ^ ");"
      in
      let style =
        {css|
      display: flex;
      flex-flow: row nowrap;

      width:100%;
      height: 1rem;
      |css}
      in
      El.div ~at:[ At.style (Jstr.v (style ^ ccs_gradient)) ] labels
    in
    let legend =
      let style =
        {css|
      display: flex;
      flex-flow: row nowrap;
      justify-content: center;

      width:100%;

      font-size: 0.75em;
      padding: 0.5em;
      |css}
      in
      let grad_style =
        "flex: 0 1 auto; width:33%; margin: 0.25em;"
        ^ "background: linear-gradient(to right, " ^ Color.to_css low_color
        ^ ", " ^ Color.to_css high_color ^ ");"
      in
      let txt_style = Jstr.v {css| flex: 0 1 auto; |css} in
      let left =
        El.div ~at:[ At.style txt_style ] [ El.txt' legend; El.txt' " 0%" ]
      in
      let right = El.div ~at:[ At.style txt_style ] [ El.txt' " 100%" ] in
      let grad = El.div ~at:[ At.style (Jstr.v grad_style) ] [] in
      El.div ~at:[ At.style (Jstr.v style) ] [ left; grad; right ]
    in
    El.div [ gradient; legend ]
end
