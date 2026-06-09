(* TODO this is currently dead code *)
let v_workdiff_bar_chart_init =
  let chart = ref None in
  fun canvas ->
    let open Chartjs in
    let options =
      let scales =
        [
          ( Jstr.v "x",
            Options.Scale.create
              ~title:
                (Jstr.v "Différence par rapport au temps ajusté en minutes")
              ~typ:(Jstr.v "linear") ~suggested_min:(-60. *. 3.)
              ~suggested_max:(60. *. 3.) () );
        ]
      in
      Options.create ~index_axis:(Jstr.v "y") ~responsive:true
        ~maintainAspectRatio:false ~animation:true ~scales ()
    in
    let d_time_diffs =
      let data = Jv.of_jv_list [] in
      Dataset.create ~label:(Jstr.v "Diff") ~border_color:(rgb 75 192 192)
        ~background_color:(rgba 75 192 192 0.2) ~tension:0.1 ~data ()
    in
    let data = Data.create ~datasets:[ d_time_diffs ] () in
    let chart_type = Jstr.v "bar" in
    let chart =
      Option.get_lazy
        (fun () ->
          let c = Chart.create ~canvas ~data ~chart_type ~options () in
          chart := Some c;
          c)
        !chart
    in
    (chart, data, d_time_diffs)
