FROM ocaml/opam

RUN sudo apt-get update \
  && sudo apt-get install m4 -y \
  && sudo rm -rf /var/lib/apt/lists/* /tmp/*

COPY --chown=opam:opam ./ $HOME/nuscr

WORKDIR $HOME/nuscr

RUN opam pin add --no-action -y nuscr.dev -k path . \
  && opam pin add --no-action -y nuscr-web.dev -k path . \
  && opam install -dt . --deps-only

RUN eval $(opam config env) \
  && dune build \
  && dune install

