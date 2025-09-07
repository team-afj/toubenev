let html =
  let open Tyxml.Html in
  html
    (head (title (txt "Béné")) [])
    (body [ script ~a:[ a_src "bundle.js"; a_script_type `Module ] (txt "") ])
