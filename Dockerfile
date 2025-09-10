# syntax=docker/dockerfile:1.4

# Stage 1
# Opam setup
#################
FROM ocaml/opam:alpine-ocaml-5.3 as build

ENV SHELL /bin/zsh
RUN sudo apk add zsh zsh-vcs
RUN wget -O /home/opam/.zshrc https://grml.org/console/zshrc
RUN sudo chown opam:opam /home/opam/.zshrc

WORKDIR /tmp/build

RUN opam init --reinit --auto-setup
RUN sudo chown -R opam:opam /tmp
# Node dependencies:
RUN sudo apk add yarn
# Install tools
RUN sudo apk add gmp-dev
RUN opam install ocaml-lsp-server ocamlformat utop --yes
RUN opam install vif --yes
# Install project dependencies
COPY --chown=opam:opam planning.opam ./
RUN opam install . --deps-only --with-test --yes

# Dev contianer:
RUN eval $(opam env)
