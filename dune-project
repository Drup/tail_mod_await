(lang dune 3.15)

(name ppx_tmaa)

(generate_opam_files true)

(source (github Drup/tail_mod_await))
(authors "Gabriel Radanne")
(maintainers "Gabriel Radanne")
(license MIT)

(package
 (name ppx_tmaa)
 (synopsis "A ppx implementing the tail mod Async/Await transformation")
 (depends
  (ocaml (>= 5.0))
   dune picos ppxlib
  (picos_mux :with-test)
  (ppx_deriving :with-test)
  (alcotest :with-test)
))
