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
  dune picos saturn
  (ppxlib (>= 0.36.0))
  (picos_mux :with-test)
  (ppx_deriving :with-test)
  (mtime :with-test)
  (alcotest :with-test)
  (bechamel :with-test)
  (bechamel-js :with-test)
  (memtrace :with-test)
  (moonpool :with-test)
))
