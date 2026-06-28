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
end

let doc_Api = Doc_API.g
let get_table ?table_id () =
  let args =
    match table_id with None -> [||] | Some id -> [| Jv.of_jstr id |]
  in
  Jv.call g "getTable" args |> Table_operations.of_jv
