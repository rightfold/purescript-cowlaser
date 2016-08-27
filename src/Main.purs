module Main
( main
) where

import Control.Alt ((<|>))
import Control.Monad.Aff (makeAff)
import Control.Monad.Reader.Class (class MonadReader)
import Control.MonadZero (class MonadZero)
import Cowlaser.HTTP (Request, Response)
import Cowlaser.Route (dir, root)
import Cowlaser.Serve (nodeHandler)
import Data.List (List(..))
import Node.HTTP (createServer, listen)
import Node.Stream (end)
import Prelude

main = do
  server <- createServer $ nodeHandler (handler1 <|> handler2)
  listen server 8080 (pure unit)

handler1 :: forall eff m. (MonadReader (Request eff) m, MonadZero m) => m (Response eff)
handler1 = do
  root
  pure { status: {code: 200, message: "OK"}
       , headers: Nil
       , body: \w -> makeAff \_ r -> end w (r unit)
       }

handler2 :: forall eff m. (MonadReader (Request eff) m, MonadZero m) => m (Response eff)
handler2 = dir "foo" $
  pure { status: {code: 500, message: "Internal Server Error"}
       , headers: Nil
       , body: \w -> makeAff \_ r -> end w (r unit)
       }
