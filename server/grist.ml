open Vif

let index req _server () =
  Response.with_file ~compression:`DEFLATE req
    (Fpath.v "./grist/widget/index.html")

let js req _server () =
  Response.with_file ~compression:`DEFLATE req
    (Fpath.v "./grist/widget/index.bc.js")
