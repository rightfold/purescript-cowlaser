module Cowlaser.HTTP
( Request
, Response
) where

import Control.Monad.Aff (Aff)
import Data.List (List)
import Node.HTTP (HTTP)
import Node.Stream (Readable, Writable)
import Prelude

type Request eff =
  { method :: String
  , uri :: String
  , headers :: List {name :: String, value :: String}
  , body :: Readable () (http :: HTTP | eff)
  }

type Response eff =
  { status :: {code :: Int, message :: String}
  , headers :: List {name :: String, value :: String}
  , body :: Writable () (http :: HTTP | eff) -> Aff (http :: HTTP | eff) Unit
  }
