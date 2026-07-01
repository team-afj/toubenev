open Brrer
open Brr
open Brr_lwd
open Brr_lwd_ui
open Lunar_jsont
open Shared
open Data_repr
open Lwd_infix

let time_select () =
  let field_desc =
    { Forms.Field.name = "time_select"; default = "12:00:00"; label = [] }
  in
  let options =
    let open Time in
    Range.(
      let iterator = iterator ~pred:(sub_minutes 15) ~succ:(add_minutes 15) in
      fold_left ~include_boundaries:false ~iterator
        (fun t acc ->
          let value = Time.to_string t in
          let name = Time.to_string ~format:`HHMM t in
          (value, name) :: acc)
        [] day)
    |> List.rev
  in
  Forms.Field_select.make field_desc (Lwd.return (Lwd_seq.of_list options))

let date_select ~first ~last () =
  let field_desc =
    {
      Forms.Field.name = "date_select";
      default = Date.to_string first;
      label = [];
    }
  in
  let options =
    let open Date in
    Range.(
      make ~first ~last
      |> fold_left ~include_boundaries:false ~iterator:iterator_day
           (fun t acc ->
             let value = Date.to_string t in
             let name = Date.to_short_string ~format:`DDMM t in
             (value, name) :: acc)
           [])
    |> List.rev
  in
  Forms.Field_select.make field_desc (Lwd.return (Lwd_seq.of_list options))

let on_site_and_available_volunteers_on_date datetime volunteers =
  Normal.Volunteers.fold volunteers ~init:(0, 0)
    ~f:(fun (acc, acc') volunteer ->
      let acc =
        if Normal.Volunteer.is_on_site_at datetime volunteer then acc + 1
        else acc
      in
      let acc' =
        if Normal.Volunteer.is_available_at datetime volunteer then acc' + 1
        else acc'
      in
      (acc, acc'))

let available_volunteers_widget (data : Rich.Planning.t) =
  let normalized = Conv.normalize data in
  let max_v = Normal.Volunteers.cardinal normalized.volunteers in
  let first', last =
    match data.infos.kind with
    | Finite dates -> (dates.start_date, dates.end_date)
  in
  let first, last = Date.(sub_days 5 first', add_days 4 last) in
  let date = date_select ~first ~last () in
  let time = time_select () in
  let result =
    Lwd.map2 (Lwd.get date.value) (Lwd.get time.value) ~f:(fun date time ->
        let date = Date.from_string_exn date in
        let time = Time.from_string_exn time in
        let datetime =
          Zoned_datetime.(from ~tz:data.infos.timezone date time)
        in
        ( time,
          on_site_and_available_volunteers_on_date datetime
            normalized.volunteers ))
  in
  let result =
    let$ time, (on_site, disponible) = result in
    let txt =
      "Bénévoles: " ^ string_of_int on_site ^ " sur site dont "
      ^ string_of_int disponible ^ " disponibles à "
      ^ Time.to_string ~format:`HHMM time
      ^ "."
    in
    El.txt' txt
  in
  let color_grad_test date =
    let date = Date.from_string_exn date in
    let color ratio =
      let b =
        Float.round ((1. -. ratio) *. 255.) |> Float.to_int |> string_of_int
      in
      "rgb(128 225 " ^ b ^ ")"
    in
    let first =
      Zoned_datetime.(
        from ~tz:data.infos.timezone date data.infos.day_start_local)
    in
    let last = Zoned_datetime.add_days 1 first in
    let range = Zoned_datetime.Range.make ~first ~last in
    let iterator =
      Zoned_datetime.(
        Range.iterator ~pred:(sub_minutes 15) ~succ:(add_minutes 15))
    in
    let ratio datetime =
      let _on_site, available =
        on_site_and_available_volunteers_on_date datetime normalized.volunteers
      in
      Float.(of_int available / of_int max_v)
    in
    let colors =
      let open Zoned_datetime in
      Range.fold_left ~include_boundaries:false ~iterator
        (fun t acc ->
          let ratio = ratio t in
          color ratio :: acc)
        [] range
    in
    let values =
      let open Zoned_datetime in
      let style = Jstr.v {css| flex: 1 1 auto; |css} in
      let tooltip placement txt =
        [
          At.v (Jstr.v "data-tooltip") (Jstr.v txt);
          At.v (Jstr.v "data-placement") (Jstr.v placement);
        ]
      in
      Range.fold_right ~include_boundaries:false ~iterator
        (fun t acc ->
          let on_site, available =
            on_site_and_available_volunteers_on_date t normalized.volunteers
          in
          let txt =
            Time.to_string ~format:`HHMM (Zoned_datetime.local_time t)
            ^ ": " ^ string_of_int on_site ^ " (" ^ string_of_int available
            ^ " dispo.) sur " ^ string_of_int max_v
          in
          let placement =
            if Zoned_datetime.(t < add_hours 6 first) then "right"
            else if Zoned_datetime.(t < add_hours 18 first) then "bottom"
            else "left"
          in
          `P (El.div ~at:(At.style style :: tooltip placement txt) []) :: acc)
        [] range
    in
    let ccs_gradient =
      "background: linear-gradient(to left, "
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
    Elwd.div ~at:[ `P (At.style (Jstr.v (style ^ ccs_gradient))) ] values
  in
  let color_grad_test = Lwd.bind (Lwd.get date.value) ~f:color_grad_test in
  Elwd.div
    [
      `R
        (Elwd.fieldset
           ~at:[ `P (At.v (Jstr.v "role") (Jstr.v "group")) ]
           [ `R date.field; `R time.field ]);
      `R result;
      `P (El.br ());
      `P (El.txt' "Résumé sur la journée:");
      `R color_grad_test;
    ]

let capacity_table ({ daily; _ } : Analysis.t) =
  let th ?tooltip v =
    let el =
      let txt = El.txt' v in
      match tooltip with
      | None -> txt
      | Some tip -> El.abbr ~at:[ At.title (Jstr.v tip) ] [ txt ]
      (* El.em ~at:[ At.v (Jstr.v "data-tooltip") (Jstr.v tip) ] [ txt ] *)
    in
    El.th [ el ]
  in
  let td ?at v = El.td ?at [ El.txt' v ] in
  let d_to_string d =
    let h, m, s = Duration.hms d in
    Printf.sprintf "%02d:%02d:%02d" h m s
  in
  let jours =
    List.rev
    @@ Date.Map.fold
         (fun d
              {
                Analysis.total_quest_time;
                total_volunteer_time;
                max_concurrent_volunteers;
                available_volunteers;
              } acc ->
           let at =
             if Duration.(total_volunteer_time < total_quest_time) then
               Some [ At.class' (Jstr.v "warn") ]
             else None
           in
           let at_av =
             if available_volunteers < max_concurrent_volunteers then
               Some [ At.class' (Jstr.v "error") ]
             else None
           in
           El.tr
             [
               td (Date.to_string d);
               td (d_to_string total_quest_time);
               td ?at (d_to_string total_volunteer_time);
               td (Int.to_string max_concurrent_volunteers);
               td ?at:at_av (Int.to_string available_volunteers);
             ]
           :: acc)
         daily []
  in
  let totals =
    let total_q, total_v =
      Date.Map.fold
        (fun _d { Analysis.total_quest_time; total_volunteer_time; _ }
             (acc_q, acc_v) ->
          Duration.(acc_q + total_quest_time, acc_v + total_volunteer_time))
        daily
        (Duration.zero, Duration.zero)
    in
    [ td "Total"; td (d_to_string total_q); td (d_to_string total_v) ]
  in
  Pico_ui.El.section
    [
      El.thead
        [
          El.tr
            [
              th "Jour";
              th ~tooltip:"Durée totale des quêtes à accomplir" "⏱️ quêtes";
              th ~tooltip:"Temps de bénévolat disponible" "⏱️👷‍♀️";
              th
                ~tooltip:
                  "Nombre maximum de bénévoles devant effecter une tâche au \
                   même moment."
                "Max 👷‍♀️";
              th
                ~tooltip:
                  "Nombre de bénévoles disponibles. Si < à \"Max 👷‍♀️\",\n\
                   le planning est impossible à résoudre."
                "#👷‍♀️";
            ];
        ];
      El.tbody jours;
      El.tfoot totals;
    ]
