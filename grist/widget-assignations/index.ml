open Brrer
open Brr
open Brr_lwd
open! Fut.Result_syntax
open! Lunar_jsont
open! Data_repr
open Tables

let on_record () =
  (* For custom widgets, add a handler that will be called whenever the row with
     the cursor changes. If the widget is correctly set this means rows from the
     SOLUTIONS table. *)
  let f =
   fun obj ->
    Console.log [ "TBN ASS DBG ON RECORD"; obj ];
    let o =
      (Jv.Int.get obj "id", Jv.Jstr.get obj "name", Jv.get obj "last_answer")
    in
    (* Lwd.set App_state.grist_solutions o; *)
    Console.log [ "TBN DBG ON RECORD PARSED"; o ]
  in
  let callback = Jv.callback ~arity:1 f in
  Grist.on_record ~callback ()

let on_records () =
  (* For custom widgets, add a handler that will be called whenever the selected
     records change. If the widget is correctly set this means rows from the
     ASSIGNATIONS table. *)
  let f =
   fun v ->
    let solutions = Solutions.ls () in
    Console.log [ "TBN ASS DBG ON RECORDS"; v; solutions ]
  in
  let callback = Jv.callback ~arity:1 f in
  Grist.on_records ~callback ()

let app = Elwd.div [ `P (El.txt' "todo") ]

let _ =
  let on_load _ =
    let () = on_record () in
    let () = on_records () in
    let root = El.find_first_by_selector (Jstr.v "main") |> Option.get in
    let app = Lwd.observe app in
    let f _ = ignore @@ Lwd.quick_sample app in
    let on_invalidate _ = ignore @@ G.request_animation_frame f in
    El.append_children root [ Lwd.quick_sample app ];
    Lwd.set_on_invalidate app on_invalidate
  in
  Ev.listen Ev.dom_content_loaded on_load (Window.as_target G.window)
