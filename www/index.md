% wreq: a Haskell web client library
% HTTP made easy for Haskell.
  <a href="tutorial.html" class="btn btn-primary btn-lg" role="button">Tutorial</a>



`wreq` is a library that makes HTTP client programming in Haskell
easy.


# Features

* Simple but powerful `lens`-based API

* Over 100 tests, and built on reliable libraries like [`http-client`](http://hackage.haskell.org/package/http-client/)
  and [`lens`](https://lens.github.io/)

* Session handling includes connection keep-alive and pooling, and
  cookie persistence

* Automatic decompression

* Powerful multipart form and file upload handling

* Support for JSON requests and responses, including navigation of
  schema-less responses

* Basic and OAuth2 bearer authentication

* Amazon Web Services (AWS) request signing (Version 4)

* AWS signing supports sending requests through the
  [Runscope Inc.](https://www.runscope.com) Traffic Inspector

# Whirlwind tour

All of the examples that follow assume that you are using the
[`OverloadedStrings`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/type-class-extensions.html#overloaded-strings)
language extension, which you can enable in `ghci` as follows:

~~~~ {.haskell}
ghci> :set -XOverloadedStrings
~~~~

And now let's get started.

~~~~ {.haskell}
ghci> import Network.Wreq
ghci> r <- get "http://httpbin.org/get"
~~~~

This library's `lens`-based API is easy to learn (the tutorial walks
you through the
[basics of lenses](tutorial.html#a-quick-lens-backgrounder) and
powerful to work with.

~~~~ {.haskell}
ghci> import Control.Lens
ghci> r ^. responseHeader "Content-Type"
"application/json"
~~~~

Safely and sanely add query parameters to URLs. Let's find the most
popular implementations of Tetris in Haskell.

~~~~ {.haskell}
ghci> let opts = defaults & param "q" .~ ["tetris"]
                          & param "language" .~ ["haskell"]
ghci> r <- getWith opts "https://api.github.com/search/code"
~~~~

Haskell-to-JSON interoperation is seamless.

~~~~ {.haskell}
ghci> import GHC.Generics
ghci> import Data.Aeson
ghci> :set -XDeriveGeneric

ghci> data Addr = Addr Int String deriving (Generic)
ghci> instance ToJSON Addr

ghci> let addr = Addr 1600 "Pennsylvania"
ghci> post "http://httpbin.org/post" (toJSON addr)
~~~~


Work easily with schemaless JSON APIs.  This traverses the complex
JSON search result we just received from GitHub above, and pulls out
the authors of our popular Tetris clones.

~~~~ {.haskell}
ghci> r ^.. responseBody . key "items" . values .
            key "owner" . key "login" . _String
["steffi2392","rmies","Spacejoker","walpen",{-...-}
~~~~

Easily write
[`attoparsec`](http://hackage.haskell.org/package/attoparsec) parsers
on the spot, to safely and reliably deal with complicated headers and
bodies.

~~~~ {.haskell}
ghci> import Data.Attoparsec.ByteString.Char8 as A
ghci> import Data.List (sort)

ghci> let comma = skipSpace >> "," >> skipSpace
ghci> let verbs = A.takeWhile isAlpha_ascii `sepBy` comma

ghci> r <- options "http://httpbin.org/get"
ghci> r ^. responseHeader "Allow" . atto verbs . to sort
ghci> ["GET","HEAD","OPTIONS"]
~~~~

There's a lot more, but why not jump in and start coding. In fact, if
you'd like to add new features, that would be great! We love pull
requests.


<div class="jumbotron" style="margin-top: 40px;">
<h2 style="margin-top: 20px;">Ready to jump in?</h2>

We've worked hard to make `wreq` quick to learn.

<a href="tutorial.html" class="btn btn-success btn-lg" role="button">Tutorial</a>

We're proud of the example-filled docs.

<a href="http://hackage.haskell.org/package/wreq" class="btn btn-info btn-lg" role="button">Documentation</a>

If you run into problems, let us know.

<a href="https://github.com/bos/wreq" class="btn btn-warning btn-lg" role="button">Issues</a>

</div>


# Acknowledgments

I'd like to thank Edward Kmett and Shachaf Ben-Kiki for tirelessly
answering my never-ending stream of
[lens](https://lens.github.io/)-related questions in `#haskell-lens`.

I also want to thank Michael Snoyman for being so quick with helpful
responses to bug reports and pull requests against his excellent
[http-client](http://hackage.haskell.org/package/http-client) package.

Finally, thanks to Kenneth Reitz for building the indispensable
[httpbin.org](http://httpbin.org/) HTTP testing service, and of course
for his [requests library](http://docs.python-requests.org/en/latest/).
