(lang dune 2.8)

(name ocaml-migrate-types)

(generate_opam_files true)

(package
 (name ocaml-migrate-types)
 (depends
  (ocaml
   (and
    (>= "4.08.0")
    (< "4.14.0")))
  ocaml-migrate-parsetree
  ppx_optcomp))
