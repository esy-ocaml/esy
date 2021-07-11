#!/usr/bin/env bash

set -euo pipefail

# install gmake, make command line on FreeBSD is BSD's make
# we need GNU make
sudo pkg install gmake ocaml-opam

WORKDIR=$PWD

# we need to setup ocaml version 4.08.1, the official package of
# FreeBSD have version 4.05.0, so let's build it from source
set -ex;
    curl -L --show-error --retry 5 -o ~/.config/ocaml-4.08.1.tar.gz https://github.com/ocaml/ocaml/archive/4.08.1.tar.gz; \
    cd ~/.config; \
    tar -xvzf ocaml-4.08.1.tar.gz; \
    cd ocaml-4.08.1; \
    # create directory for ocaml prefix
    mkdir -p ~/.config/esy-ocaml; \
    ./configure CC=cc -disable-cfi --prefix="${HOME}/.config/esy-ocaml"; \
    gmake -j4 world.opt; \
    gmake install

# set the PATH to our compiled directory, so we have our ocaml version
export PATH=~/.config/esy-ocaml/bin:$PATH

# answer yes to all yes/no questions without prompting
export OPAMYES="true"

#initialize opam and create switch
set -ex; \
    ocaml --version; \
    opam init; \
    opam switch create esy-build ocaml-system.4.08.1

set -ex; \
    cd $WORKDIR; \
    opam install \
        "dune=2.2.0" \
        "yojson=1.7.0" \
        "reason=3.5.2" \
        "re=1.9.0" \
        "ppx_sexp_conv=v0.13.0" \
        "ppx_let=v0.13.0" \
        "ppx_inline_test=v0.13.0" \
        "ppx_expect=v0.13.0" \
        "ppx_deriving_yojson=3.5.1" \
        "ppx_deriving=4.4" \
        "opam-state=2.0.5" \
        "opam-format=2.0.5" \
        "opam-file-format=2.0.0" \
        "opam-core=2.0.5" \
        "ocamlformat=0.14.0" \
        "menhir=20200211" \
        "lwt_ppx=1.2.4" \
        "lwt=4.4.0" \
        "logs=0.7.0" \
        "lambda-term=2.0.2" \
        "fpath=0.7.2" \
        "fmt=0.8.8" \
        "dose3=5.0.1" \
        "cudf=0.9" \
        "bos=0.2.0" \
    # pinned
    opam pin pastel "https://github.com/facebookexperimental/reason-native.git"; \
    opam pin cli "https://github.com/facebookexperimental/reason-native.git"; \
    opam pin file-context-printer "https://github.com/facebookexperimental/reason-native.git"; \
    opam pin rely "https://github.com/facebookexperimental/reason-native.git"; \
    opam pin cmdliner "https://github.com/esy-ocaml/cmdliner.git#e9316bc"; \
    opam pin angstrom "https://github.com/esy-ocaml/angstrom.git#5a06a0"; \
    # then build
    dune build -p "esy,esy-build-package" 