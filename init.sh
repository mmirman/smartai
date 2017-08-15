#!/bin/bash

git submodule update --init --recursive

./tensorflow-haskell/tools/install_macos_dependencies.sh

cabal install tensorflow-haskell/tensorflow tensorflow-haskell/tensorflow-proto tensorflow-haskell/tensorflow-opgen/ tensorflow-haskell/tensorflow-ops tensorflow-haskell/tensorflow-core-ops tensorflow-haskell/tensorflow-logging/ tensorflow-haskell/tensorflow-records tensorflow-haskell/tensorflow-records-conduit

cabal install
