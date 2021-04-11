(lang dune 2.8)

(name ocaml-migrate-types)

(generate_opam_files true)

(package
 (name ocaml-migrate-types)
 (depends
  (ocaml
   (and
    (>= "4.10.0")
    (< "4.13.0")))
  ocaml-migrate-parstree))
