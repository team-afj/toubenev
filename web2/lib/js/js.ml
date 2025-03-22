open! Brr

module Date = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"

  let global = Jv.(get global "Date")
  let of_string s : t = Jv.new' global [| Jv.of_string s |]
  let of_int s : t = Jv.new' global [| Jv.of_int s |]
end
