(lang dune 3.17)

(name mariocaml)

(generate_opam_files)

(package
 (name mariocaml)
 (allow_empty)
 (synopsis "Browser-based Mario platformer in OCaml")
 (description
  "MariOCaml is an HTML5 canvas implementation of Super Mario Bros mechanics, compiled to JavaScript with js_of_ocaml.")
 (depends
  (ocaml (>= 4.14))
  dune
  (js_of_ocaml (>= 4.1))
  (js_of_ocaml-compiler (>= 4.1))
  (js_of_ocaml-ppx (>= 4.1))))
