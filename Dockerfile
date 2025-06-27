FROM ocaml/opam:archlinux-ocaml-5.3

COPY --chown=opam:opam Artifact-Overview.md LICENSE Makefile Readme.md bench dune-project examples lib ppx_tmaa.opam test ./tail_mod_await/

USER opam

RUN sudo pacman -Syu

RUN sudo pacman -S jq python python-matplotlib gmp pkgconf firefox

WORKDIR tail_mod_await/

RUN opam install --deps --with-test . && eval $(opam env)

RUN opam switch create --no-switch memtrace_viewer --packages ocaml.5.1.1,codicons,memtrace_viewer.v0.17.0

ENTRYPOINT ["opam", "exec", "--"]

CMD bash
