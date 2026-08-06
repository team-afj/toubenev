open Brrer
open Brr
open Brr_lwd
open Brr_lwd_ui
open Lunar_jsont
open Shared
open Data_repr
open Rich
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
  let _dates_range = Date.Range.make ~first ~last in
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
  let module Color = Brr.El.Style.Color in
  let c_indispo = Color.make 232 57 41 in
  let c_dispo = Color.make 140 247 93 in
  let make_day_gradient date =
    let data =
      let date = Date.from_string_exn date in
      let first =
        Zoned_datetime.(
          from ~tz:data.infos.timezone date data.infos.day_start_local)
      in
      let last = Zoned_datetime.add_days 1 first in
      let ratio datetime =
        let on_site, available =
          on_site_and_available_volunteers_on_date datetime
            normalized.volunteers
        in
        Float.(of_int available / of_int on_site)
      in
      let iterator =
        Zoned_datetime.(
          Range.iterator ~pred:(sub_minutes 15) ~succ:(add_minutes 15))
      in
      let range = Zoned_datetime.Range.make ~first ~last in
      Zoned_datetime.Range.fold_right ~include_boundaries:false ~iterator
        (fun t acc ->
          let ratio = ratio t in
          let on_site, available =
            on_site_and_available_volunteers_on_date t normalized.volunteers
          in
          let label =
            Time.to_string ~format:`HHMM (Zoned_datetime.local_time t)
            ^ ": " ^ string_of_int on_site ^ " (" ^ string_of_int available
            ^ " dispo.) sur " ^ string_of_int max_v
          in
          (ratio, label) :: acc)
        [] range
    in
    Data_viz.Linear_gradient.make ~low_color:c_indispo ~high_color:c_dispo
      ~legend:"Disponibilité" data
  in
  let color_grad = Lwd.map (Lwd.get date.value) ~f:make_day_gradient in
  Elwd.div
    [
      `R
        (Elwd.fieldset
           ~at:[ `P (At.v (Jstr.v "role") (Jstr.v "group")) ]
           [ `R date.field; `R time.field ]);
      `R result;
      `P (El.br ());
      `P (El.txt' "Résumé sur la journée:");
      `R color_grad;
    ]

let mk_capacity_table lines footer =
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
      El.tbody lines;
      El.tfoot [ footer ];
    ]

let d_to_string d =
  let h, m, s = Duration.hms d in
  Printf.sprintf "%02d:%02d:%02d" h m s

let td ?at v = El.td ?at [ El.txt' v ]

let mk_day day total_quest_time total_volunteer_time max_concurrent_volunteers
    available_volunteers =
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
      td (Date.to_string day);
      td (d_to_string total_quest_time);
      td ?at (d_to_string total_volunteer_time);
      td (Int.to_string max_concurrent_volunteers);
      td ?at:at_av (Int.to_string available_volunteers);
    ]

let capacity_table ({ daily; _ } : Analysis.t) =
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
           mk_day d total_quest_time total_volunteer_time
             max_concurrent_volunteers available_volunteers
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
    El.tr [ td "Total"; td (d_to_string total_q); td (d_to_string total_v) ]
  in
  mk_capacity_table jours totals

let capacity_table_for_tt (task_type : Task_type.t)
    ({ state = { data_rich; _ }; data; static_analysis } :
      Tables.Solutions.normal) =
  let open Normal in
  let quests =
    Quests.filter
      (fun q ->
        Option.map_or ~default:false
          (Task_type.equal task_type)
          q.initial.task_type)
      data.quests
  in
  let volunteers =
    Volunteers.filter
      (fun v -> Task_type.Set.mem task_type v.skills)
      data.volunteers
  in
  let total_q = ref Duration.zero in
  let total_v = ref Duration.zero in
  let jours =
    List.rev
    @@ Date.Map.fold
         (fun day quests acc ->
           let total_quest_time =
             Quests.fold ~init:0
               ~f:(fun acc q -> acc + Quest.weighted_duration ~unit:`Minutes q)
               quests
             |> Duration.from_minutes
           in
           (total_q := Duration.(!total_q + total_quest_time));
           let volunteers =
             let day' = day in
             Volunteers.filter
               (fun v ->
                 match (v.initial.arrival, v.initial.departure) with
                 | None, None -> true
                 | Some arrival, None ->
                     Date.(Zoned_datetime.local_date arrival <= day')
                 | None, Some departure ->
                     Date.(day' <= Zoned_datetime.local_date departure)
                 | Some arrival, Some departure ->
                     Date.(Zoned_datetime.local_date arrival <= day')
                     && Date.(day' <= Zoned_datetime.local_date departure))
               volunteers
           in
           let total_volunteer_time =
             Volunteers.fold volunteers ~init:Duration.zero ~f:(fun acc v ->
                 let theoretical_load =
                   Workload_analysis.theoretical_load static_analysis ~of_:v
                     ~on:day quests
                   |> function
                   | `Fixed load | `Flexible load -> load
                 in
                 Duration.(acc + theoretical_load))
           in
           (total_v := Duration.(!total_v + total_volunteer_time));
           let max_concurrent_volunteers =
             (* Classical two-steps algorithm for max interval overlap. Sort the list by
       start time and with additional weights. Then sweep the list to accumulate
       the weight and remember the maximum. *)
             let events =
               Quests.fold quests ~init:Zoned_datetime.Map.empty
                 ~f:(fun acc q ->
                   let required_volunteers = q.initial.required_volunteers in
                   let add_delta acc time delta =
                     Zoned_datetime.Map.update time
                       (function
                         | None -> Some delta
                         | Some existing_delta -> Some (existing_delta + delta))
                       acc
                   in
                   let acc = add_delta acc q.slot.start required_volunteers in
                   add_delta acc (Time_slot.end_ q.slot) (-required_volunteers))
             in
             Zoned_datetime.Map.fold
               (fun _time delta (current_active, current_peak) ->
                 let current_active = current_active + delta in
                 (current_active, max current_peak current_active))
               events (0, 0)
             |> snd
           in
           let available_volunteers = Volunteers.cardinal volunteers in
           mk_day day total_quest_time total_volunteer_time
             max_concurrent_volunteers available_volunteers
           :: acc)
         (quests_by_day data_rich.infos quests)
         []
  in
  let totals =
    El.tr [ td "Total"; td (d_to_string !total_q); td (d_to_string !total_v) ]
  in
  mk_capacity_table jours totals

let capacity_table
    ({ state = { analysis; data_rich; _ }; _ } as s : Tables.Solutions.normal) =
  let rev_type = Hashtbl.create (CCRAL.length data_rich.task_types) in
  let type_select =
    let field_desc =
      { Forms.Field.name = "cap_type_select"; default = "#ALL"; label = [] }
    in
    let options =
      CCRAL.fold data_rich.task_types
        ~x:[ ("#ALL", "Tous les types") ]
        ~f:(fun acc tt ->
          if tt.Task_type.specialist_only then begin
            let id = id_to_string tt.id in
            Hashtbl.add rev_type id tt;
            (id, tt.name) :: acc
          end
          else acc)
      |> List.rev
    in
    Forms.Field_select.make field_desc (Lwd.return (Lwd_seq.of_list options))
  in
  let tbl =
    let$ type_id = Lwd.get type_select.value in
    match Hashtbl.find_opt rev_type type_id with
    | None -> capacity_table analysis
    | Some tt -> capacity_table_for_tt tt s
  in
  Elwd.div [ `R type_select.field; `R tbl ]

open Normal

let fifteen_minutes = Duration.from_minutes 15

let per_volunteer (assignations : Api.assignation list) (v : Normal.Volunteer.t)
    =
  let quests =
    List.filter_map assignations ~f:(fun { Api.quest; volunteers } ->
        if Volunteers.mem v volunteers then Some quest else None)
  in
  let tt_good, tt_bad, tt_neutral =
    List.fold_left quests ~init:(0, 0, 0) ~f:(fun (gd, bd, ne) q ->
        match q.Quest.initial.task_type with
        | None -> (gd, bd, ne + 1)
        | Some tt ->
            if Task_type.Set.mem tt v.wanted_tasks then (gd + 1, bd, ne)
            else if Task_type.Set.mem tt v.unwanted_tasks then (gd, bd + 1, ne)
            else (gd, bd, ne + 1))
  in
  let good15, bad15, neutral15 =
    List.fold_left quests ~init:(0, 0, 0) ~f:(fun (gd, bd, ne) q ->
        let quest_end = Time_slot.end_ q.Quest.slot in
        let rec loop acc current =
          if Zoned_datetime.(current >= quest_end) then acc
          else
            let next_time =
              Zoned_datetime.(min (current + fifteen_minutes) quest_end)
            in
            let block_duration =
              Duration.(
                Zoned_datetime.to_local_duration next_time
                - Zoned_datetime.to_local_duration current)
            in
            let current_block =
              { Time_slot.start = current; duration = block_duration }
            in
            let preferences_score =
              List.fold_left v.preferences ~init:(gd, bd, ne)
                ~f:(fun (gd, bd, ne) (pref_score, pref_slot) ->
                  if Time_slot.overlaps current_block pref_slot then
                    if pref_score > 0 then (gd + 1, bd, ne) else (gd, bd + 1, ne)
                  else (gd, bd, ne + 1))
            in
            loop preferences_score next_time
        in
        loop (0, 0, 0) q.slot.start)
  in
  let txt_tt =
    Printf.sprintf "Types de quêtes: %i 😃 %i 😕 %i 🙂" tt_good tt_bad tt_neutral
  in
  let txt_15 =
    Printf.sprintf "Quarts d'heures: %i 😃 %i 😕 %i 🙂" good15 bad15 neutral15
  in
  El.div [ El.div [ El.txt' txt_tt ]; El.div [ El.txt' txt_15 ] ]

let per_volunteer (data : Api.data) (assignations : Api.assignation list) =
  let rev_v = Hashtbl.create (Volunteers.cardinal data.volunteers) in
  let v_select =
    let options =
      Volunteers.fold data.volunteers ~init:[] ~f:(fun acc v ->
          Hashtbl.add rev_v v.id v;
          (v.id, v.name) :: acc)
      |> List.sort ~cmp:(fun (_, a) (_, b) ->
          String.compare (String.lowercase_ascii a) (String.lowercase_ascii b))
    in
    let field_desc =
      let default = Option.map_or ~default:"" fst (List.head_opt options) in
      { Forms.Field.name = "infos_v_select"; default; label = [] }
    in
    Forms.Field_select.make field_desc (Lwd.return (Lwd_seq.of_list options))
  in
  let v_infos =
    Lwd.map (Lwd.get v_select.value) ~f:(fun v_id ->
        match Hashtbl.find_opt rev_v v_id with
        | Some v -> per_volunteer assignations v
        | None -> El.nbsp ())
  in
  Elwd.div [ `R v_select.field; `R v_infos ]
