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
    let queue = Flux.Bqueue.(create with_close 10) in
    let fill =
      let counter = ref 0 in
      let rec aux () =
        Miou.yield ();
        Flux.Bqueue.put queue @@ Printf.sprintf "data: ping %i\n\n" !counter;
        incr counter;
        Miou_unix.sleep 2.;
        aux ()
      in
      Miou.async aux
    in
    let src = Flux.Source.bqueue queue in
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
  (* TODO generic static files serving ? *)
  (* TODO auth and doc identification *)
  [
    get (rel /?? nil) --> default;
    get (rel / "bundle.js" /?? nil) --> js;
    get (rel / "sse" /?? nil) --> sse;
    get (rel / "grist" /?? any) --> Grist.index;
    get (rel / "grist" / "index.html" /?? nil) --> Grist.index;
    get (rel / "grist" / "index.bc.js" /?? nil) --> Grist.js;
    options (rel / "grist" / "data" /?? nil) --> Grist.preflight;
    put
      (Type.json_encoding Grist_import.data_jsont)
      (rel / "grist" / "data" /?? nil)
    --> Grist.handle_put_data;
  ]

let () = Logs.set_level ~all:true (Some Debug)

let _cfg =
  (* TODO This was an attempt to force h2 usage but there seems to be an issue
     with TLS right-now. See https://github.com/robur-coop/vif/issues/11 *)
  let tls =
    let open Result in
    let* cert =
      Unix.openfile "certs/certificate.pem" [ O_RDONLY ] 0
      |> Unix.in_channel_of_descr |> In_channel.input_all
      |> X509.Certificate.decode_pem
    in
    let* key =
      Unix.openfile "certs/privatekey.pem" [ O_RDONLY ] 0
      |> Unix.in_channel_of_descr |> In_channel.input_all
      |> X509.Private_key.decode_pem
    in
    let certificates = `Single ([ cert ], key) in
    Tls.Config.server ~certificates ~alpn_protocols:[ "h2" ] ()
  in
  match tls with
  | Error (`Msg err) ->
      Logs.err (fun l -> l "TLS config error: %S" err);
      exit 1
  | Ok tls ->
      let sockaddr = Unix.(ADDR_INET (Unix.inet_addr_loopback, 1357)) in
      Vif.config ~http:(`H2 H2.Config.default) ~tls sockaddr

let cfg =
  let sockaddr = Unix.(ADDR_INET (Unix.inet_addr_loopback, 1357)) in
  Vif.config sockaddr

let () =
  Fmt_tty.setup_std_outputs ~utf_8:true ();
  Logs.set_reporter (Log_config.reporter Fmt.stderr);
  Miou_unix.run @@ fun () -> Vif.run ~cfg routes ()
