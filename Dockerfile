FROM ocaml/opam:archlinux-ocaml-5.3

COPY --chown=opam:opam . ./tail_mod_await/

USER opam

RUN sudo pacman -Syu --noconfirm

RUN sudo pacman -S jq python python-matplotlib gmp pkgconf --noconfirm

WORKDIR tail_mod_await/

RUN opam install --deps --with-test . && eval $(opam env)

ENTRYPOINT ["opam", "exec", "--"]

CMD bash
