module Cowlaser.HTTP
( Request
, Response
) where

import Control.Monad.Aff (Aff)
import Data.List (List)
import Data.Map (Map)
import Data.NonEmpty (NonEmpty)
import Data.String.CaseInsensitive (CI)
import Node.HTTP (HTTP)
import Node.Stream (Readable, Writable)
import Prelude

type Request eff =
  { method :: CI
  , uri :: String
  , headers :: Map CI (NonEmpty List String)
  , body :: Readable () (http :: HTTP | eff)
  }

type Response eff =
  { status :: {code :: Int, message :: String}
  , headers :: Map CI (NonEmpty List String)
  , body :: Writable () (http :: HTTP | eff) -> Aff (http :: HTTP | eff) Unit
  }
