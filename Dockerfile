FROM ocaml/opam

RUN sudo apt-get update \
  && sudo apt-get install m4 python3 libgmp-dev -y \
  && sudo rm -rf /var/lib/apt/lists/* /tmp/*

COPY --chown=opam:opam ./ $HOME/nuscr

WORKDIR $HOME/nuscr

RUN opam install -y ./nuscr.opam

CMD ["nuscr"]
