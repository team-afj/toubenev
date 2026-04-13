module Data : sig
  module Row_records : sig
    include Jv.CONV

    val by_row : t -> Jv.t
  end
end

module Doc_API : sig
  include Jv.CONV

  val fetch_table : table_id:Jstr.t -> Data.Row_records.t Fut.or_error
end

val doc_Api : Doc_API.t
