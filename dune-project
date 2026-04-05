(lang dune 3.15)

(generate_opam_files)

(source
 (github team-afj/toubenev))

(license AGPL-3.0)

(authors "Ulysse Gérard")

(maintainers "")

(package
 (allow_empty)
 (name planning)
 (synopsis "A live planning and exchange tool for events")
 (description "A live planning and exchange tool for events")
 (depends
  (ocaml
   (>= 5.3))
  containers
  js_of_ocaml
  pyml
  brr
  lwd
  brr-lwd
  vif
  flux
  ISO8601
  calendar
  (jsont
   (>= "0.1.1"))
  ppx_deriving_jsont
  ppxlib
  bytesrw
  ; Vendored lwd deps:
  js_of_ocaml-lwt
  tyxml
  ; Vendored brr-lwd-ui deps:
  uuidm
  hector
  containers-data
  base64
  ; OR Tools
  ortools
  ortools_solvers))
