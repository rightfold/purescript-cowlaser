module Main (main) where

import Control.Monad.Eff (Eff)
import Cowlaser.HTTP (Response)
import Cowlaser.Serve (nodeHandler)
import Data.List (List(..))
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (createServer, HTTP, listen)
import Node.Stream.Aff (end, writeString)
import Prelude


handler :: forall eff m. (Applicative m) => m (Response eff)
handler = pure { status: {code: 200, message: "OK"}
               , headers: Nil
               , body: \w -> do
                   writeString w UTF8 "Hello, world!"
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
