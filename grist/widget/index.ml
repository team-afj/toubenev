open Brr

module Js = struct
  module Object = struct
    external array_of_obj_from_obj_of_arrays : Jv.t -> Jv.t
      = "array_of_obj_from_obj_of_arrays"
  end
end

module Grist : sig
  module Data : sig
    module Row_records : sig
      include Jv.CONV

      val by_row : t -> Jv.t
    end
  end

  module Doc_API : sig
    include Jv.CONV

    val fetch_table : table_id:Jstr.t -> t -> Data.Row_records.t Fut.or_error
  end

  val doc_API : Doc_API.t
end = struct
  let g = Jv.get Jv.global "grist"
  let () = Console.error [ g ]

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

    let fetch_table ~table_id t =
      Jv.call t "fetchTable" [| Jv.of_jstr table_id |]
      |> Fut.of_promise ~ok:Data.Row_records.of_jv
  end

  let doc_API = Jv.get g "docApi" |> Doc_API.of_jv
end

let _ =
  let open Grist in
  let open Fut.Result_syntax in
  let+ records =
    Doc_API.fetch_table doc_API ~table_id:(Jstr.v "Types_de_quete")
  in
  Console.log [ "POUET POUET" ];
  Console.log [ Data.Row_records.by_row records ]
