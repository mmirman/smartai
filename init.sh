git submodule update --init --recursive

cabal sandbox init
cabal sandbox add-source tensorflow-haskell/tensorflow-proto
cabal sandbox add-source tensorflow-haskell/tensorflow-queue
cabal sandbox add-source tensorflow-haskell/tensorflow-ops
cabal sandbox add-source tensorflow-haskell/tensorflow-opgen
cabal sandbox add-source tensorflow-haskell/tensorflow-logging
cabal sandbox add-source tensorflow-haskell/tensorflow-mnist
cabal sandbox add-source tensorflow-haskell/tensorflow-mnist-input-data
cabal sandbox add-source tensorflow-haskell/tensorflow-records
cabal sandbox add-source tensorflow-haskell/tensorflow-records-conduit
cabal sandbox add-source tensorflow-haskell/tensorflow

cabal install --dependencies-only
