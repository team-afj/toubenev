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
  El.append_children (Document.body G.document) [ iframe ];
  ignore
  @@ Ev.listen Ev.load
       (fun _ ->
         let i_win = Jv.get (El.to_jv iframe) "contentWindow" |> Window.of_jv in
         let i_doc = Jv.get (Window.to_jv i_win) "document" |> Document.of_jv in
         let body = Document.body i_doc in
         El.append_children body [ el ];
         G.set_timeout ~ms:250 (fun () -> Window.print i_win) |> ignore)
       (El.as_target iframe)

let modal () =
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
    let peek () =
      List.filter_map ~f:Fun.id [ Lwd.peek bp_var; Lwd.peek bt_var ]
    in
    ([ `R bp.element; `R bt.element ], peek)
  in
  let footer =
    let cancel =
      Elwd.button
        ~ev:[ `P (Elwd.handler Ev.click (fun _ -> Lwd.set show_modal false)) ]
        [ `P (El.txt' "Annuler") ]
    in
    let print =
      let on_click _ =
        Lwd.peek App_state.last_answer
        |> Option.iter @@ fun { App_state.data_rich; answer; _ } ->
           let planning =
             Render.make_plannings data_rich answer (peek_options ())
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
