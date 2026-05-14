open Vif

(* TODO is this satisfying ? *)
let allow_origin () = Response.add ~field:"Access-Control-Allow-Origin" "*"

let preflight _req _server () =
  let open Response.Syntax in
  let* () = Response.empty in
  let* () = allow_origin () in

  let* () = Response.add ~field:"Access-Control-Allow-Credentials" "true" in
  let* () = Response.add ~field:"Access-Control-Allow-Headers" "*" in
  let* () = Response.add ~field:"Access-Control-Allow-Methods" "GET,PUT" in
  let* () = Response.add ~field:"Vary" "Origin" in
  Response.respond `OK
