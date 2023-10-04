(lang dune 2.9)
(name gml)
(version 0.1)
(using menhir 2.1)

(generate_opam_files true)
(source (github smuller/gml-checker))
(license MIT)

(package
 (name gml)
 (synopsis "the gml typechecker")
 (depends
  (ocaml (= 4.14.0))
  menhir
  ppx_deriving)
 (depopts
  (ocaml-lsp-server (= 1.15.1-4.14))
  ocamlformat))
