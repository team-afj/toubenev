open Brr
open Brr_lwd_ui
open Brr_lwd
open Shared
open Lunar_jsont
open Data_repr
open Normal
open Analysis

let print_signed_int = Int.to_string

let color_grad i =
  At.style (Jstr.v ("background-color: hwb(9 " ^ string_of_int i ^ "% 0%);"))

let max_red = 200

let make_red i =
  let i = max 0 (min max_red (abs i)) in
  let i = i * 100 / max_red in
  color_grad (100 - i)

let make_table (analysis : t) real_time =
  let head =
    Date.Map.fold
      (fun date _ acc ->
        El.th [ El.txt' (Date.to_short_string ~format:`DDMM date) ] :: acc)
      analysis.daily
      [ El.th [ El.txt' "Total" ]; El.th [] ]
    |> List.rev |> El.tr
  in
  let volunteer (v : Volunteer.t) (facts : volunteer_analyses) =
    let name = El.th [ El.txt' v.name ] in
    let total_diff, total =
      let expected_load =
        if real_time then facts.event.theoretical_load
        else facts.event.adjusted_load
      in
      let diff =
        Duration.(facts.event.actual_load - expected_load |> to_minutes)
      in
      ( diff,
        El.td
          ~at:[ make_red diff ]
          [
            El.txt' (print_signed_int diff);
            El.txt' (" (" ^ Duration.to_string expected_load ^ ")");
          ] )
    in
    ( total_diff,
      Date.Map.fold
        (fun _ facts acc ->
          let expected_load =
            if real_time then facts.theoretical_load else facts.adjusted_load
          in
          let diff =
            Duration.(facts.actual_load - expected_load |> to_minutes)
          in
          El.td
            ~at:[ make_red diff ]
            [
              El.txt' (print_signed_int diff);
              El.txt' (" (" ^ Duration.to_string expected_load ^ ")");
            ]
          :: acc)
        facts.daily [ total; name ]
      |> List.rev |> El.tr )
  in
  let volunteers =
    Volunteer.Map.fold
      (fun v f acc -> volunteer v f :: acc)
      analysis.volunteers []
    |> List.sort ~cmp:(fun (d1, _) (d2, _) -> Int.compare d2 d1)
    |> List.map ~f:snd
  in
  El.table ~at:[ Pico_ui.At.overflow_auto ] (head :: volunteers)

let make (analysis : t) =
  let { element; desc = Check { state; _ } } =
    Forms.Field_checkboxes.make_single
      {
        value = ();
        id = "theoretical_or_adjusted_diff_switch";
        name = "theoretical_or_adjusted_diff_switch";
        label =
          (fun () ->
            [
              `P
                (El.txt'
                   "Montrer les temps prévus en théorie plutôt que les temps \
                    \"ajustés\"");
            ]);
        state = false;
      }
  in
  let table =
    Lwd.map (Lwd.get state) ~f:(fun s -> make_table analysis (Option.is_some s))
  in
  Elwd.div [ `R element; `R table ]
