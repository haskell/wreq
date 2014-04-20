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
minutes: a few seconds for `wreq`, and the rest for its dependencies.


# Interactive usage

We'll run our examples interactively via the `ghci` shell.

~~~~
$ ghci
~~~~

To start using `wreq`, we import the `Network.Wreq` module.

~~~~ {.haskell}
ghci> import Network.Wreq
ghci> r <- get "http://httpbin.org/get"
ghci> :type r
r :: Response ByteString
~~~~

The variable `r` above is the
[`Response`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#t:Response)
from the server.


# A quick lens backgrounder

The `wreq` package makes heavy use of Edward Kmett's
[`lens`](https://lens.github.io/) package to provide a clean,
consistent API.

~~~~ {.haskell}
ghci> import Control.Lens
~~~~

While `lens` has a vast surface area, the portion that you must
understand in order to productively use `wreq` is tiny.

A lens provides a way to focus on a portion of a Haskell value. For
example, the `Response` type has a
[`responseStatus`](http://hackage.haskell.org/package/wreq/docs/Network-Wreq.html#v:responseStatus)
lens, which focuses on the status information returned by the server.

~~~~ {.haskell}
ghci> r ^. responseStatus
Status {statusCode = 200, statusMessage = "OK"}
~~~~

The
[`^.`](http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#v:-94-.)
operator takes a value as its first argument, a lens as its second,
and returns the portion of the value focused on by the lens.

We compose lenses using function composition, which allows us to
easily focus on part of a deeply nested structure.

~~~~ {.haskell}
ghci> r ^. responseStatus . statusCode
200
~~~~

We'll have more to say about lenses as this tutorial proceeds.


# Adding parameters to a URL

~~~~ {.haskell}
ghci> let opts = defaults & param "foo" .~ ["bar"]
ghci> getWith opts "http://httpbin.org"
~~~~
