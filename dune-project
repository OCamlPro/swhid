(lang dune 2.8)

(name swhids)

(license ISC)

(authors "OCamlPro <contact@ocamlpro.com>")

(source
 (github ocamlpro/swhids))

(generate_opam_files true)

(using menhir 2.1)

(package
 (name swhids)
 (synopsis "OCaml library to work with Software Heritage identifiers")
 (description
  "swhids is an OCaml library to work with Software Heritage identifiers")
 (depends
  (ocaml
   (>= 4.0.5))
  (dune
   (>= 2.8))
  (menhir
   (>= 2.1))
  (bisect_ppx
   (and
    :with-test
    (>= 2.6)
    :dev))))
