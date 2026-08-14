open Brrer
open Brr
open Brr_lwd

let print el =
  let doc = Document.find_el_by_id G.document (Jstr.v "print-iframe") in
  Option.iter El.remove doc;
  let iframe =
    El.iframe
      ~at:
        [ At.src (Jstr.v "plannings.html"); At.style (Jstr.v "display: none;") ]
      []
  in
  El.prepend_children (Document.body G.document) [ iframe ];
  ignore
  @@ Ev.listen Ev.load
       (fun _ ->
         let i_win = Jv.get (El.to_jv iframe) "contentWindow" |> Window.of_jv in
         let i_doc = Jv.get (Window.to_jv i_win) "document" |> Document.of_jv in
         let body = Document.body i_doc in
         El.append_children body [ el ];
         G.set_timeout ~ms:250 (fun () -> Window.print i_win) |> ignore)
       (El.as_target iframe)

let modal get_solution =
  let show_modal = Lwd.var false in
  let options, peek_options =
    let bp =
      Forms.Field_checkboxes.make_single
        {
          value = `By_place;
          id = "chk-by-place";
          name = "By place";
          label = (fun _ -> [ `P (El.txt' "Plannings par lieu") ]);
          state = true;
        }
    in
    let bp_var =
      let (Check { state; _ }) = bp.desc in
      state
    in
    let bt =
      Forms.Field_checkboxes.make_single
        {
          value = `By_quest_kind;
          id = "chk-by-task";
          name = "By task";
          label = (fun _ -> [ `P (El.txt' "Plannings par type de quête") ]);
          state = false;
        }
    in
    let bt_var =
      let (Check { state; _ }) = bt.desc in
      state
    in
    let list_all =
      Forms.Field_checkboxes.make_single
        {
          value = `List_all_tasks;
          id = "chk-lst-all";
          name = "List all";
          label = (fun _ -> [ `P (El.txt' "Liste de toutes les quêtes") ]);
          state = false;
        }
    in
    let list_all_var =
      let (Check { state; _ }) = list_all.desc in
      state
    in
    let list_by_v =
      Forms.Field_checkboxes.make_single
        {
          value = `List_tasks_by_volunteer;
          id = "chk-lst-by-v";
          name = "List by v";
          label =
            (fun _ -> [ `P (El.txt' "Liste des quêtes de chaque bénévole") ]);
          state = false;
        }
    in
    let list_by_v_var =
      let (Check { state; _ }) = list_by_v.desc in
      state
    in
    let gp =
      Forms.Field_checkboxes.make_single
        {
          value = `Daily_grids;
          id = "chk-grid-planning";
          name = "Grid planning";
          label = (fun _ -> [ `P (El.txt' "Grilles quotidiennes") ]);
          state = true;
        }
    in
    let gp_var =
      let (Check { state; _ }) = gp.desc in
      state
    in
    let details =
      Forms.Field_checkboxes.make_single
        {
          value = ();
          id = "chk-details";
          name = "details";
          label =
            (fun _ ->
              [
                `P
                  (El.txt'
                     "Afficher les nom de quêtes en cas d'ambiguité. (sinon \
                      seul le slug 🐌 est utilisé)");
              ]);
          state = false;
        }
    in
    let details_var =
      let (Check { state; _ }) = details.desc in
      state
    in
    let peek () =
      ( List.filter_map ~f:Fun.id
          [
            Lwd.peek bp_var;
            Lwd.peek bt_var;
            Lwd.peek list_all_var;
            Lwd.peek list_by_v_var;
            Lwd.peek gp_var;
          ],
        Lwd.peek details_var |> Option.is_some )
    in
    ( [
        `P (El.legend [ El.txt' "Type(s) de planning à imprimer :" ]);
        `R bp.element;
        `R bt.element;
        `R list_all.element;
        `R list_by_v.element;
        `R gp.element;
        `P (El.legend [ El.txt' "Autres options :" ]);
        `R details.element;
      ],
      peek )
  in
  let footer =
    let cancel =
      Elwd.button
        ~ev:[ `P (Elwd.handler Ev.click (fun _ -> Lwd.set show_modal false)) ]
        [ `P (El.txt' "Annuler") ]
    in
    let print =
      let on_click _ =
        get_solution ()
        |> Option.iter @@ fun { Tables.Solutions.data_rich; answer; _ } ->
           let planning =
             let sections, details = peek_options () in
             Render.make_plannings ~details data_rich answer sections
           in
           Lwd.set show_modal false;
           print planning
      in
      Elwd.button
        ~ev:[ `P (Elwd.handler Ev.click on_click) ]
        [ `P (El.txt' "Imprimer") ]
    in
    [ `R cancel; `R print ]
  in
  Pico_ui.Elwd.modal ~opened:show_modal
    ~title:(`P (El.txt' "Options d'impression"))
    ~footer options
