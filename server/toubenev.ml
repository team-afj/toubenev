open Vif

let default req _server () =
  let open Vif.Response.Syntax in
  let field = "content-type" in
  let* () = Vif.Response.add ~field "text/html; charset=utf-8" in
  let* () = Vif.Response.with_tyxml req Views.Home.html in
  Vif.Response.respond `OK

let js req _server () =
  Vif.Response.with_file ~compression:`DEFLATE req (Fpath.v "./www/bundle.js")

let sse =
  let counter = ref 0 in
  fun req _server () ->
    let sse_connection_number = !counter in
    Logs.debug (fun l -> l "New SSE connection #%i" sse_connection_number);
    incr counter;
    let open Response.Syntax in
    let queue = Stream.Bqueue.create 10 in
    let fill =
      let counter = ref 0 in
      let rec aux () =
        Miou.yield ();
        Stream.Bqueue.put queue @@ Printf.sprintf "data: ping %i\n\n" !counter;
        incr counter;
        Miou_unix.sleep 2.;
        aux ()
      in
      Miou.async aux
    in
    let src = Stream.Source.of_bqueue queue in
    let* () =
      let field = "content-type" in
      Response.add ~field "text/event-stream"
    in
    let* () = Response.with_source req src in
    let* () = Response.respond `OK in
    Logs.debug (fun fmt ->
        fmt "SSE connection closed #%i" sse_connection_number);
    Miou.cancel fill;
    Response.return ()

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  [
    get (rel /?? nil) --> default;
    get (rel / "bundle.js" /?? nil) --> js;
    get (rel / "sse" /?? nil) --> sse;
  ]

let () = Logs.set_level ~all:true (Some Debug)
let () = Miou_unix.run @@ fun () -> Vif.run routes ()
