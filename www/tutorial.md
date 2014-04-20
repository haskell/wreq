% A wreq tutorial

# Installation

To use the `wreq` package, simply use `cabal`, the standard Haskell
package management command.

~~~~
cabal update
cabal install -j --disable-tests wreq
~~~~

Depending on how many prerequisites you already have installed, and
what your Cabal configuration looks like, the build may take a few
minutes: a few seconds for `wreq`, and the rest for the dependencies.

To give you a flavour, I installed `wreq` from scratch in a
Cabal sandbox on two different machines.

* Without library profiling, the build took *1min 43sec* on an
  8-core 3.4GHz Core i7 Linux box running GHC 7.6.3.

* Again without library profiling, the elapsed time was *4min 55sec*
  on a 4-core 2.2GHz Macbook Pro running GHC 7.6.3.

* With library profiling enabled on the same Macbook Pro, the elapsed
  time increased to *8min 18sec*.
