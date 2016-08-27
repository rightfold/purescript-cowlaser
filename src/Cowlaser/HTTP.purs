module Cowlaser.HTTP
( Request
, Response

, headerValues
) where

import Control.Monad.Aff (Aff)
import Data.List (List(..))
import Data.String.CaseInsensitive (CI)
import Node.HTTP (HTTP)
import Node.Stream (Readable, Writable)
import Prelude

type Request eff =
  { method :: String
  , uri :: String
  , headers :: List {name :: CI, value :: String}
  , body :: Readable () (http :: HTTP | eff)
  }

type Response eff =
  { status :: {code :: Int, message :: String}
  , headers :: List {name :: CI, value :: String}
  , body :: Writable () (http :: HTTP | eff) -> Aff (http :: HTTP | eff) Unit
  }

-- | Return the header values in a request or response given the name of a
-- | header. Header names are case-insensitive, as per the HTTP specification.
-- | The order of the values in the result is unspecified.
headerValues
  :: forall r
   . CI
  -> {headers :: List {name :: CI, value :: String} | r}
  -> List String
headerValues lookfor = go Nil <<< _.headers
  where go acc Nil = acc
        go acc (Cons {name, value} rest)
          | name == lookfor = go (Cons value acc) rest
          | otherwise = go acc rest
