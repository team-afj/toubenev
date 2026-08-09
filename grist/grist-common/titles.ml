open Tables
open Fut.Result_syntax

(* TODO: This is kinda hackish since we modify a internal, undocumented,
     table. It can break anytime. *)
let table_id = Jstr.v "_grist_Views_section"
let widget_base_name = "Solver link"
let widget_base_name_j = Jstr.v "Solver link"

(* This function updates the title of item number [id]
     in "_grist_Views_section" *)
let meta_update_title ~ids new_title =
  let open Grist in
  let views_section_table = get_table ~table_id () in
  let records =
    let title = Jstr.v "title" in
    let new_title = Jv.of_string new_title in
    List.map ids ~f:(fun id -> Record.v ~id ~fields:[| (title, new_title) |] ())
  in
  Table_operations.update views_section_table ~records ()

(* List all uses the the widget by searching in the section titles in
     [_grist_Views_section].
     TODO: this is not very robust. *)
let all_widget_uses () =
  let+ rows = Data.fetch table_id in
  Jv.to_list
    (fun row ->
      let id = Jv.get row "id" |> Jv.to_int in
      let title_jv = Jv.get row "title" in
      if Jv.is_null title_jv || Jv.is_undefined title_jv then None
      else if
        Jv.call title_jv "includes" [| Jv.of_jstr widget_base_name_j |]
        |> Jv.to_bool
      then Some id
      else None)
    rows
  |> List.filter_map ~f:Fun.id

let update_prefixes prefix =
  let* ids = all_widget_uses () in
  meta_update_title ~ids @@ prefix ^ " " ^ widget_base_name
