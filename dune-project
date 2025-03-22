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
  containers
  js_of_ocaml
  brr
  lwd
  brr-lwd
  ISO8601
  calendar
  (jsont
   (>= "0.1.1"))
  ; Vendored ppx_deriving_jsont deps:
  ppxlib))
