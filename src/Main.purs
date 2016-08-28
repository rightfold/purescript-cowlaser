module Main (main) where

import Control.Monad.Eff (Eff)
import Control.Monad.Reader.Class (ask, class MonadReader)
import Cowlaser.HTTP (Request, Response)
import Cowlaser.Params (query, query')
import Cowlaser.Serve (nodeHandler)
import Data.List (List(..))
import Data.NonEmpty ((:|))
import Data.Map as Map
import Data.String.CaseInsensitive (CI(..))
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (createServer, HTTP, listen)
import Node.Stream.Aff (end, writeString)
import Prelude


handler :: forall eff m. (MonadReader (Request eff) m) => m (Response eff)
handler = do
  (req :: Request eff) <- ask
  xs <- query "x"
  xm <- query' "x"
  pure { status: {code: 200, message: "OK"}
       , headers: Map.singleton (CI "Content-Type") ("text/html" :| Nil)
       , body: \w -> do
           writeString w UTF8 ("<pre>" <> show req.uri <> "</pre>")
           writeString w UTF8 ("<pre>" <> show req.headers <> "</pre>")
           writeString w UTF8 ("<pre>" <> show xs <> "</pre>")
           writeString w UTF8 ("<pre>" <> show xm <> "</pre>")
           end w
       }

main :: forall eff. Eff (http :: HTTP | eff) Unit
main = do
  server <- createServer $ nodeHandler handler
  listen server 8080 (pure unit)


{-
module Main
( main
) where

import Control.Alt ((<|>))
import Control.Monad.Reader.Class (class MonadReader)
import Control.MonadZero (class MonadZero)
import Cowlaser.HTTP (Request, Response)
import Cowlaser.Route (dir, root, withRouting)
import Cowlaser.Serve (nodeHandler)
import Data.List (List(..))
import Node.HTTP (createServer, listen)
import Node.Stream.Aff (end)
import Prelude

main = do
  server <- createServer $ nodeHandler (withRouting $ handler1 <|> handler2)
  listen server 8080 (pure unit)

handler1 :: forall eff m. (MonadReader (Request eff) m, MonadZero m) => m (Response eff)
handler1 = do
  root
  pure { status: {code: 200, message: "OK"}
       , headers: Nil
       , body: \w -> end w
       }

handler2 :: forall eff m. (MonadReader (Request eff) m, MonadZero m) => m (Response eff)
handler2 = dir "foo" $
  pure { status: {code: 500, message: "Internal Server Error"}
       , headers: Nil
       , body: \w -> end w
       }
-}
