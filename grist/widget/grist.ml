module Js = struct
  module Object = struct
    external array_of_obj_from_obj_of_arrays : Jv.t -> Jv.t
      = "array_of_obj_from_obj_of_arrays"
  end
end

let g = Jv.get Jv.global "grist"

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
