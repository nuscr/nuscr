(lang dune 1.11)

(name nuscr)

(using menhir 1.0)

(using fmt 1.2)

(generate_opam_files true)

(source
 (github nuscr/nuscr))

(license GPL-3.0-or-later)

(authors "Francisco Ferreira" "Fangyi Zhou" "Simon Castellan")

(maintainers "Francisco Ferreira")

(homepage "https://nuscr.github.io/nuscr/")

(documentation "https://nuscr.github.io/nuscr/docs/")

(package
 (name nuscr)
 (synopsis
   "A tool to manipulate and validate Scribble-style multiparty protocols")
 (description
   "A toolkit to manipulate Scribble-style multiparty protocols, based on classical multiparty session type theory. The toolkit provides means to define global protocols, project to local protocols, convert local protocols to a CFSM representation, and generate OCaml code for protocol implementations.")
 (depends
  (menhir
   (and
    :build
    (>= 20190924)))
  (ppx_deriving
   (and
    :build
    (>= 4.4)))
  (dune
   (and
    :build
    (>= 1.11.4)))
  (base
   (>= v0.12.0))
  (stdio
   (>= v0.12.0))
  (ppx_sexp_conv
   (>= v0.12.0))
  (ppx_inline_test :with-test)
  (ocamlgraph
   (>= 1.8.8))
  (js_of_ocaml
   (>= 3.5.0))
  (js_of_ocaml-ppx
   (>= 3.5.0))
  (js_of_ocaml-tyxml
   (>= 3.5.0))
  (ppxlib
   (and
    (>= 0.8.1)
    (< 0.9.0)))))
