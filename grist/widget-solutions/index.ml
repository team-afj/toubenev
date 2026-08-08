open Brrer
open Brr
open Brr_lwd
open! Lwd_infix
open! Fut.Result_syntax
open! Lunar_jsont
open! Data_repr
open! Rich
open! Normal
open! Tables

(* This widget should be associated to the ASSIGNATION table and filtered by
   SOLUTION *)

module App_state = struct
  let selected_in_grist : int Lwd.var = Lwd.var 1

  let solutions : (int * string * Jstr.t) Lwd_seq.t Lwd.var =
    Lwd.var Lwd_seq.empty

  let sol_select =
    let options =
      Lwd.get solutions
      |> Lwd_seq.map (fun (id, name, _) -> (string_of_int id, name))
    in
    let field_desc =
      let default = "1" in
      { Forms.Field.name = "sol_solutions_select"; default; label = [] }
    in
    Forms.Field_select.make field_desc options

  let selected_solution = Lwd.get sol_select.value
end

let grist_set_pointer id =
  let pos = Jv.obj [| ("rowId", Jv.of_int (int_of_string id)) |] in
  (* This requires ready({allowSelectBy:true}) *)
  Grist.set_cursor_pos ~pos

let get_solution id : (int * string * Jstr.t) option =
  Lwd_seq.to_list (Lwd.peek App_state.solutions)
  |> List.find_opt ~f:(fun (i, _, _) -> id = i)

let now () =
  Unix.time () |> Float.to_int |> Duration.from_seconds
  |> Zoned_datetime.from_duration
  |> Zoned_datetime.change_timezone ~tz:(Timezone.make ~hour:2 ~min:0)
  |> Zoned_datetime.to_local_datetime

let copy_from id =
  let id = int_of_string id in
  let _, _name, data = get_solution id |> Option.get_exn_or "bad solution" in
  let* assignations = Tables.Assignations.get_assignations ~solution:id in
  let assignations =
    List.map assignations ~f:(fun obj ->
        ( Jv.get obj "initial_quest",
          Jv.get obj "ref",
          Jv.get obj "start",
          Jv.get obj "end_",
          Jv.get obj "volunteers",
          Jv.get obj "assigned_volunteers",
          Jv.get obj "keep" ))
  in
  let now = now () |> Datetime.to_string in
  let+ res = Solutions.create ~name:(Jstr.v now) data in
  let solution = Jv.Int.get (Jv.Jarray.get res 0) "id" in
  Tables.Assignations.create_solution_assignations ~solution assignations

let delete ~solution =
  let* _ = Tables.Assignations.remove_assignations ~solution in
  Tables.Solutions.remove ~solution

let app =
  let solution_manager =
    let focus_btn =
      let disabled =
        Lwd.map2 (Lwd.get App_state.selected_in_grist)
          App_state.selected_solution ~f:(fun g f ->
            if g = int_of_string f then At.disabled else At.void)
      in
      let handler =
        Elwd.handler Ev.click (fun _ ->
            ignore @@ grist_set_pointer (Lwd.peek App_state.sol_select.value))
      in
      Pico_ui.Elwd.button
        ~at:[ `R disabled ]
        ~ev:[ `P handler ]
        [ `P (El.txt' "Select in Grist") ]
    in
    let copy_btn =
      let in_progress = Lwd.var false in
      let disabled =
        Lwd.map (Lwd.get in_progress) ~f:(function
          | true -> At.disabled
          | false -> At.void)
      in
      Pico_ui.Elwd.button
        ~at:[ `R disabled ]
        ~ev:
          [
            `P
              (Elwd.handler Ev.click (fun _ ->
                   Lwd.set in_progress true;
                   Fut.await (copy_from (Lwd.peek App_state.sol_select.value))
                   @@ fun _ -> Lwd.set in_progress false));
          ]
        [ `P (El.txt' "Copy") ]
    in
    let delete_btn =
      let in_progress = Lwd.var false in
      let disabled =
        Lwd.map2 App_state.selected_solution (Lwd.get in_progress)
          ~f:(fun i p ->
            if p || int_of_string i <= 2 then At.disabled else At.void)
      in
      Pico_ui.Elwd.button
        ~at:[ `R disabled ]
        ~ev:
          [
            `P
              (Elwd.handler Ev.click (fun _ ->
                   Lwd.set in_progress true;
                   let solution =
                     int_of_string (Lwd.peek App_state.sol_select.value)
                   in
                   Fut.await (delete ~solution) @@ fun _ ->
                   Lwd.set in_progress false));
          ]
        [ `P (El.txt' "Delete") ]
    in
    Elwd.div
      [
        `R App_state.sol_select.field; `R focus_btn; `R copy_btn; `R delete_btn;
      ]
  in
  Elwd.div [ `R solution_manager ]

let decode_solution_jv sol =
  let sol_id = Jv.Int.get sol "id" in
  let sol_name = Jv.Jstr.get sol "name" |> Jstr.to_string in
  let sol_raw = Jv.Jstr.get sol "last_answer" in
  (sol_id, sol_name, sol_raw)

let on_record () =
  let f =
   fun v ->
    let id = Jv.Int.get v "id" in
    Lwd.set App_state.sol_select.value (string_of_int id);
    Lwd.set App_state.selected_in_grist id
  in
  let callback = Jv.callback ~arity:1 f in
  let options =
    Jv.obj [| ("keepEncoded", Jv.false'); ("expandRefs", Jv.false') |]
  in
  Grist.on_record ~callback ~options ()

let on_records () =
  let f =
   fun v ->
    let sols = Jv.to_list decode_solution_jv v in
    (* TODO update active solution *)
    (* let current = Lwd.peek App_state.sol_select.value in *)
    Lwd.set App_state.solutions (Lwd_seq.of_list sols)
  in
  let callback = Jv.callback ~arity:1 f in
  let options =
    Jv.obj [| ("keepEncoded", Jv.false'); ("expandRefs", Jv.false') |]
  in
  Grist.on_records ~callback ~options ()

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
