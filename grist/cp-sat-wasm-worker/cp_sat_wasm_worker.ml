[@@@warning "-32-34"]

(* This web worker runs the wasm version of ortools. *)

module Uint8_array = struct
  type t = Jv.t

  external to_jv : t -> Jv.t = "%identity"
  external of_jv : Jv.t -> t = "%identity"
  external of_string : string -> t = "cp_sat_wasm_uint8_array_of_string"
  external to_string : t -> string = "cp_sat_wasm_string_of_uint8_array"
end

module Cp_sat = struct
  type uint8_array = Uint8_array.t
  type solve_result = Jv.t
  type validation_result = Jv.t

  external solve_raw_promise : Jv.t -> Jv.t -> Jv.Promise.t
    = "cp_sat_wasm_solve_raw"

  external solve_promise : Jv.t -> Jv.t -> Jv.Promise.t = "cp_sat_wasm_solve"
  external validate_promise : Jv.t -> Jv.Promise.t = "cp_sat_wasm_validate"

  let solve_raw ?params model =
    let params =
      match params with
      | None -> Jv.null
      | Some params -> Uint8_array.to_jv params
    in
    solve_raw_promise (Uint8_array.to_jv model) params
    |> Fut.of_promise ~ok:Uint8_array.of_jv

  let solve ?params model =
    let params =
      match params with
      | None -> Jv.null
      | Some params -> Uint8_array.to_jv params
    in
    solve_promise (Uint8_array.to_jv model) params |> Fut.of_promise ~ok:Fun.id

  let validate model =
    validate_promise (Uint8_array.to_jv model) |> Fut.of_promise ~ok:Fun.id

  let response_bytes result = Jv.get result "bytes" |> Uint8_array.of_jv
  let response_proto result = Jv.get result "response"
  let validation_ok result = Jv.get result "ok" |> Jv.to_bool
  let validation_message result = Jv.get result "message" |> Jv.to_string
end

module Test = struct
  type result = Ortools.Sat.Response.t * int * int

  let encode_pb encode value =
    let encoder = Pbrt.Encoder.create () in
    encode value encoder;
    Pbrt.Encoder.to_string encoder

  let solve_response model response_pb =
    let decoder = Pbrt.Decoder.of_string response_pb in
    Ortools.Sat.Response.pb_decode model decoder

  let small_model () =
    let open Ortools.Sat in
    let model = make ~name:"cp-sat-wasm-worker-test" () in
    let x = Var.new_int model ~lb:0 ~ub:10 "x" in
    let y = Var.new_int model ~lb:0 ~ub:10 "y" in
    add model (var x + var y == of_int 7);
    add model (var x - var y == of_int 1);
    maximize model ((2 * x) + var y);
    (model, x, y)

  let solve_small_model_with_wasm () =
    let model, x, y = small_model () in
    let model_pb = encode_pb Ortools.Sat.pb_encode model in
    let parameters_pb =
      encode_pb Ortools.Sat.Parameters.pb_encode
        (Ortools.Sat.Parameters.defaults ())
    in
    Cp_sat.solve_raw
      ~params:(Uint8_array.of_string parameters_pb)
      (Uint8_array.of_string model_pb)
    |> Fut.map (function
      | Ok response_pb ->
          let response =
            solve_response model (Uint8_array.to_string response_pb)
          in
          let x_value = response.solution.(Ortools.Sat.Var.to_index x) in
          let y_value = response.solution.(Ortools.Sat.Var.to_index y) in
          (response, x_value, y_value)
      | Error err -> failwith (Jv.Error.message err |> Jstr.to_string))
end

let () = Brr.Console.log [ "ETST"; Test.solve_small_model_with_wasm () ]
