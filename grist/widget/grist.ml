module Js = struct
  module Object = struct
    external array_of_obj_from_obj_of_arrays : Jv.t -> Jv.t
      = "array_of_obj_from_obj_of_arrays"
  end
end

let g = Jv.get Jv.global "grist"
let fut_unit p = Fut.of_promise ~ok:(fun _ -> ()) p
let fut_jv p = Fut.of_promise ~ok:Fun.id p
let fut_jstr p = Fut.of_promise ~ok:Jv.to_jstr p
let fut_jstr_list p = Fut.of_promise ~ok:(Jv.to_list Jv.to_jstr) p
let jv_to_option jv = Jv.to_option Fun.id jv

module Access_token_result = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  let token t = Jv.get (to_jv t) "token" |> Jv.to_jstr
  let base_url t = Jv.get (to_jv t) "baseUrl" |> Jv.to_jstr

  let ttl_msec t =
    Jv.get (to_jv t) "ttlMsec" |> fun ttl ->
    if Jv.is_undefined ttl || Jv.is_null ttl then None else Some (Jv.to_int ttl)
end

let fields_to_jv fs =
  Jv.obj (Array.map ~f:(fun (k, v) -> (Jstr.to_string k, v)) fs)

module New_record = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  let v ?fields () =
    match fields with
    | None -> Jv.obj [||]
    | Some fs -> Jv.obj [| ("fields", fields_to_jv fs) |]
end

module Record = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  let v ~id ?fields () =
    match fields with
    | None -> Jv.obj [| ("id", Jv.of_int id) |]
    | Some fs -> Jv.obj [| ("id", Jv.of_int id); ("fields", fields_to_jv fs) |]
end

module Add_or_update_record = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  let v ~require ?require_id ?fields () =
    let require_obj = fields_to_jv require in
    (match require_id with
    | None -> ()
    | Some id -> Jv.set require_obj "id" (Jv.of_int id));
    match fields with
    | None -> Jv.obj [| ("require", require_obj) |]
    | Some fs ->
        Jv.obj [| ("require", require_obj); ("fields", fields_to_jv fs) |]
end

module Op_options = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  let v ?parse_strings () =
    let jv = Jv.obj [||] in
    (match parse_strings with
    | None -> ()
    | Some b -> Jv.set jv "parseStrings" (Jv.of_bool b));
    jv
end

type on_many = All | None_ | First

let on_many_to_string = function
  | All -> "all"
  | None_ -> "none"
  | First -> "first"

module Upsert_options = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  let v ?add ?update ?on_many ?allow_empty_require ?parse_strings () =
    let jv = Jv.obj [||] in
    (match add with None -> () | Some b -> Jv.set jv "add" (Jv.of_bool b));
    (match update with
    | None -> ()
    | Some b -> Jv.set jv "update" (Jv.of_bool b));
    (match on_many with
    | None -> ()
    | Some m -> Jv.set jv "onMany" (Jv.of_string (on_many_to_string m)));
    (match allow_empty_require with
    | None -> ()
    | Some b -> Jv.set jv "allowEmptyRequire" (Jv.of_bool b));
    (match parse_strings with
    | None -> ()
    | Some b -> Jv.set jv "parseStrings" (Jv.of_bool b));
    jv
end

module Table_operations = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  let create t ~records ?options () =
    let records_jv = Jv.of_list New_record.to_jv records in
    let args =
      match options with
      | None -> [| records_jv |]
      | Some options -> [| records_jv; Op_options.to_jv options |]
    in
    Jv.call (to_jv t) "create" args |> fut_jv

  let destroy t ~record_ids =
    Jv.call (to_jv t) "destroy" [| Jv.of_list Jv.of_int record_ids |]
    |> fut_unit

  let get_table_id t = Jv.call (to_jv t) "getTableId" [||] |> fut_jstr

  let update t ~records ?options () =
    let records_jv = Jv.of_list Record.to_jv records in
    let args =
      match options with
      | None -> [| records_jv |]
      | Some options -> [| records_jv; Op_options.to_jv options |]
    in
    Jv.call (to_jv t) "update" args |> fut_unit

  let upsert t ~records ?options () =
    let records_jv = Jv.of_list Add_or_update_record.to_jv records in
    let args =
      match options with
      | None -> [| records_jv |]
      | Some options -> [| records_jv; Upsert_options.to_jv options |]
    in
    Jv.call (to_jv t) "upsert" args |> fut_unit
end

module View_API = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  let allow_select_by t = Jv.call (to_jv t) "allowSelectBy" [||] |> fut_unit

  let fetch_selected_record t ~row_id ?options () =
    let args =
      match options with
      | None -> [| Jv.of_int row_id |]
      | Some options -> [| Jv.of_int row_id; options |]
    in
    Jv.call (to_jv t) "fetchSelectedRecord" args |> fut_jv

  let fetch_selected_table t ?options () =
    let args =
      match options with None -> [||] | Some options -> [| options |]
    in
    Jv.call (to_jv t) "fetchSelectedTable" args |> fut_jv

  let set_cursor_pos t ~pos =
    Jv.call (to_jv t) "setCursorPos" [| pos |] |> fut_unit

  let set_selected_rows t ~row_ids =
    Jv.call (to_jv t) "setSelectedRows" [| row_ids |] |> fut_unit
end

module Data = struct
  module Row_records = struct
    type t = Jv.t

    external to_jv : t -> Jv.t = "%identity"
    external of_jv : Jv.t -> t = "%identity"

    let by_row t = Js.Object.array_of_obj_from_obj_of_arrays t
  end
end

module Doc_API = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  let g = Jv.get g "docApi" |> of_jv

  let fetch_table ~table_id =
    Jv.call g "fetchTable" [| Jv.of_jstr table_id |]
    |> Fut.of_promise ~ok:Data.Row_records.of_jv

  let fetch_selected_record t ~row_id ?options () =
    View_API.fetch_selected_record (View_API.of_jv t) ~row_id ?options ()

  let fetch_selected_table t ?options () =
    View_API.fetch_selected_table (View_API.of_jv t) ?options ()

  let allow_select_by t = View_API.allow_select_by (View_API.of_jv t)
  let set_cursor_pos t ~pos = View_API.set_cursor_pos (View_API.of_jv t) ~pos

  let set_selected_rows t ~row_ids =
    View_API.set_selected_rows (View_API.of_jv t) ~row_ids

  let apply_user_actions t ~actions ?options () =
    let args =
      match options with
      | None -> [| actions |]
      | Some options -> [| actions; options |]
    in
    Jv.call t "applyUserActions" args |> fut_jv

  let get_access_token t ?options () =
    let args =
      match options with None -> [||] | Some options -> [| options |]
    in
    Jv.call t "getAccessToken" args
    |> Fut.of_promise ~ok:Access_token_result.of_jv

  let get_doc_name t = Jv.call t "getDocName" [||] |> fut_jstr
  let list_tables t = Jv.call t "listTables" [||] |> fut_jstr_list
end

module Section_API = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  let g = Jv.get g "sectionApi" |> of_jv

  let configure t ~custom_options =
    Jv.call (to_jv t) "configure" [| custom_options |] |> fut_unit

  let mappings t =
    Jv.call (to_jv t) "mappings" [||] |> Fut.of_promise ~ok:jv_to_option
end

module Widget_API = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"

  let g = Jv.get g "widgetApi" |> of_jv
  let clear_options t = Jv.call (to_jv t) "clearOptions" [||] |> fut_unit

  let get_option t ~key =
    Jv.call (to_jv t) "getOption" [| Jv.of_jstr key |] |> fut_jv

  let get_options t =
    Jv.call (to_jv t) "getOptions" [||] |> Fut.of_promise ~ok:jv_to_option

  let set_option t ~key ~value =
    Jv.call (to_jv t) "setOption" [| Jv.of_jstr key; value |] |> fut_unit

  let set_options t ~options =
    Jv.call (to_jv t) "setOptions" [| options |] |> fut_unit
end

let doc_Api = Doc_API.g
let view_Api = Jv.get g "viewApi" |> View_API.of_jv
let section_Api = Section_API.g
let widget_Api = Widget_API.g
let selected_table = Jv.get g "selectedTable" |> Table_operations.of_jv
let checkers = Jv.get g "checkers"

let get_table ?table_id () =
  let args =
    match table_id with None -> [||] | Some id -> [| Jv.of_jstr id |]
  in
  Jv.call g "getTable" args |> Table_operations.of_jv

let fetch_selected_record ~row_id ?options () =
  let args =
    match options with
    | None -> [| Jv.of_int row_id |]
    | Some options -> [| Jv.of_int row_id; options |]
  in
  Jv.call g "fetchSelectedRecord" args |> fut_jv

let fetch_selected_table ?options () =
  let args =
    match options with None -> [||] | Some options -> [| options |]
  in
  Jv.call g "fetchSelectedTable" args |> fut_jv

let allow_select_by () = Jv.call g "allowSelectBy" [||] |> fut_unit

let get_access_token ?options () =
  let args =
    match options with None -> [||] | Some options -> [| options |]
  in
  Jv.call g "getAccessToken" args
  |> Fut.of_promise ~ok:Access_token_result.of_jv

let clear_options () = Jv.call g "clearOptions" [||] |> fut_unit
let get_option ~key = Jv.call g "getOption" [| Jv.of_jstr key |] |> fut_jv

let get_options () =
  Jv.call g "getOptions" [||] |> Fut.of_promise ~ok:jv_to_option

let set_option ~key ~value =
  Jv.call g "setOption" [| Jv.of_jstr key; value |] |> fut_unit

let set_options ~options = Jv.call g "setOptions" [| options |] |> fut_unit

let map_column_names ~data ?options () =
  let args =
    match options with
    | None -> [| data |]
    | Some options -> [| data; options |]
  in
  Jv.call g "mapColumnNames" args

let map_column_names_back ~data ?options () =
  let args =
    match options with
    | None -> [| data |]
    | Some options -> [| data; options |]
  in
  Jv.call g "mapColumnNamesBack" args

let on_any ~event_name ~listener = Jv.call g "on" [| event_name; listener |]

let on ~event_name ~listener =
  on_any ~event_name:(Jv.of_jstr event_name) ~listener

let on_new_record ~callback = Jv.call g "onNewRecord" [| callback |] |> ignore
let on_options ~callback = Jv.call g "onOptions" [| callback |] |> ignore

let on_record ~callback ?options () =
  let args =
    match options with
    | None -> [| callback |]
    | Some options -> [| callback; options |]
  in
  Jv.call g "onRecord" args |> ignore

let on_records ~callback ?options () =
  let args =
    match options with
    | None -> [| callback |]
    | Some options -> [| callback; options |]
  in
  Jv.call g "onRecords" args |> ignore

let ready ?settings () =
  let args =
    match settings with None -> [||] | Some settings -> [| settings |]
  in
  Jv.call g "ready" args |> ignore

let set_cursor_pos ~pos = Jv.call g "setCursorPos" [| pos |] |> fut_unit

let set_selected_rows ~row_ids =
  Jv.call g "setSelectedRows" [| row_ids |] |> fut_unit
