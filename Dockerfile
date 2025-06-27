FROM ocaml/opam:archlinux-ocaml-5.3

COPY --chown=opam:opam . ./tail_mod_await/

USER opam

RUN sudo pacman -Syu --noconfirm

RUN sudo pacman -S jq python python-matplotlib gmp pkgconf firefox --noconfirm

WORKDIR tail_mod_await/

RUN opam install --deps --with-test . && eval $(opam env)

RUN opam switch create --no-switch memtrace_viewer --packages ocaml.5.1.1,codicons,memtrace_viewer.v0.17.0

ENTRYPOINT ["opam", "exec", "--"]

CMD bash
