(lang dune 2.8)

(name nuscr)

(cram enable)

(using menhir 2.0)

(generate_opam_files true)

(source
 (github nuscr/nuscr))

(license GPL-3.0-or-later)

(authors "Francisco Ferreira" "Fangyi Zhou" "Simon Castellan" "Benito Echarren")

(maintainers "Francisco Ferreira")

(homepage "https://nuscr.dev/")

(documentation "https://nuscr.dev/nuscr/docs/")

(package
 (name nuscr)
 (synopsis
   "A tool to manipulate and validate Scribble-style multiparty protocols")
 (description
   "A toolkit to manipulate Scribble-style multiparty protocols, based on classical multiparty session type theory. The toolkit provides means to define global protocols, project to local protocols, convert local protocols to a CFSM representation, and generate OCaml code for protocol implementations.")
 (depends
  (ocaml
   (>= 4.10))
  (menhir
   (>= 20190924))
  (sedlex
   (>= 2.5))
  (ppx_deriving
   (>= 5.2))
  dune
  (base
   (>= v0.12.0))
  (stdio
   (>= v0.12.0))
  (ppx_sexp_conv
   (>= v0.12.0))
  (ppx_here
   (>= v0.12.0))
  (z3 :with-test)
  (odoc :with-doc)
  (ocamlgraph
   (>= 1.8.8))
  (ppxlib
   (>= 0.22.0))
  (cmdliner
   (>= 1.1.0))
  (process
   (>= 0.2.1))
  (ocaml-protoc-plugin
   (>= 4.2.0))
 )
)

(package
 (name nuscr-web)
 (synopsis
   "A tool to manipulate and validate Scribble-style multiparty protocols")
 (description
   "A toolkit to manipulate Scribble-style multiparty protocols, based on classical multiparty session type theory. The toolkit provides means to define global protocols, project to local protocols, convert local protocols to a CFSM representation, and generate OCaml code for protocol implementations.")
 (depends
  (nuscr
   (= :version))
  dune
  (js_of_ocaml
   (>= 3.5.0))
  (js_of_ocaml-ppx
   (>= 3.5.0))
  (js_of_ocaml-tyxml
   (>= 3.5.0))))
