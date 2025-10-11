open Brrer
open Brr
open Brr_lwd
open Brr_lwd_ui
module Event_source = Brr_io.Event_source

let event_source = Event_source.create ~url:(Jstr.v "/sse") ()

let _ =
  Ev.listen Brr_io.Message.Ev.message
    (fun ev -> Console.log [ ev ])
    (Event_source.as_target event_source)

let columns = Lwd.return (Lwd_seq.element (Table.Columns.v "n1" "" []))
let _layout = { Table.columns; status = []; row_height = Utils.Unit.Rem 2. }
let source_rows = Lwd_table.make ()
let () = Lwd_table.append' source_rows "toto"

(* let data_source =
  {
    Table.Virtual_bis.total_items = Lwd.return 4;
    source_rows;
    render =
      (fun row _ ->
        let v = Lwd_table.get row |> Option.get_or ~default:"def" in

        Lwd.return (Lwd_seq.element (Lwd.return (El.txt' v))));
  }
 *)
let tbl = assert false
let app = Elwd.div [ `R tbl ]

let app_container =
  El.find_first_by_selector (Jstr.v "#app") |> Option.get_exn_or ""

let _filters_ui =
  let on_load _ =
    let app = Lwd.observe @@ app in
    let on_invalidate _ =
      ignore @@ G.request_animation_frame
      @@ fun _ -> ignore @@ Lwd.quick_sample app
    in
    El.append_children app_container [ Lwd.quick_sample app ];
    Lwd.set_on_invalidate app on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)
