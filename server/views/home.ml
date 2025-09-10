let html =
  let open Tyxml.Html in
  html
    (head (title (txt "Béné")) [])
    (body
       [
         div ~a:[ a_id "app" ] [];
         script ~a:[ a_src "bundle.js"; a_script_type `Module ] (txt "");
       ])
