open Brr

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
         Console.error [ "LOAD" ];
         let i_win = Jv.get (El.to_jv iframe) "contentWindow" |> Window.of_jv in
         let i_doc = Jv.get (Window.to_jv i_win) "document" |> Document.of_jv in
         (* let i_doc =
           Jv.get (El.to_jv iframe) "contentDocument" |> Document.of_jv
         in *)
         (* let head = Document.head i_doc in *)
         let body = Document.body i_doc in
         El.append_children body [ el ];
         Window.print i_win)
       (El.as_target iframe)
