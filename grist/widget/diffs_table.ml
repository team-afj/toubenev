open Brr
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

let make (analysis : t) =
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
      let diff =
        Duration.(
          facts.event.actual_load - facts.event.adjusted_load |> to_minutes)
      in
      (diff, El.td ~at:[ make_red diff ] [ El.txt' (print_signed_int diff) ])
    in
    ( total_diff,
      Date.Map.fold
        (fun _ facts acc ->
          let diff =
            Duration.(facts.actual_load - facts.adjusted_load |> to_minutes)
          in
          El.td ~at:[ make_red diff ] [ El.txt' (print_signed_int diff) ] :: acc)
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
