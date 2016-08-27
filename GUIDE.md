# Cowlaser Guide

Cowlaser is a PureScript library with utilities for building server-side web
applications. To use Cowlaser, you must be familiar with at least the following
PureScript features:

 - building and running PureScript code;
 - row polymorphism;
 - transformers; and
 - possibly more.

## Installation

If you don't have a `bower.json` file yet, create it:

    echo '{"name":"cowlaser-example"}' > bower.json

To install Cowlaser, run the following command:

    bower install --save purescript-cowlaser

## Hello, world!

Let's start off with a simple hello world application. It won't do anything
exciting but it will help you understand the basics, and verify that your
setup is OK.

First, you need some extra PureScript libraries:

    bower install --save purescript-{eff,lists,node-buffer,node-http,prelude}

Now create a file called `src/Main.purs`, and add some boilerplate:

```purescript
module Main (main) where

import Control.Monad.Eff (Eff)
import Cowlaser.HTTP (Response)
import Cowlaser.Serve (nodeHandler)
import Data.List (List(..))
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (createServer, HTTP, listen)
import Node.Stream.Aff (end, writeString)
import Prelude
```

The request handler is a computation that returns an HTTP response. Our example
will read thus:

```purescript
handler :: forall eff m. (Applicative m) => m (Response eff)
handler = pure { status: {code: 200, message: "OK"}
               , headers: Nil
               , body: \w -> do
                   writeString w UTF8 "Hello, world!"
                   end w
               }
```

Then we create a server and listen:

```purescript
main :: forall eff. Eff (http :: HTTP | eff) Unit
main = do
  server <- createServer $ nodeHandler handler
  listen server 8080 (pure unit)
```

And that's all we need! You can now run the application:

    pulp build && node -e 'require("./output/Main").main()'

and visit it at http://localhost:8080/.
