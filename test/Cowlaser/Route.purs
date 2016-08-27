module Test.Cowlaser.Route (main) where

import Control.Alt ((<|>))
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Cowlaser.Route (dir)
import Data.Maybe (Maybe(..))
import Prelude
import Test.Assert (assert)

main = do
  assert $ runReaderT handler {uri: "/"} == Just "nope"
  assert $ runReaderT handler {uri: "/foo"} == Just ""
  assert $ runReaderT handler {uri: "/foo/bar"} == Just "/bar"
  assert $ runReaderT handler {uri: "/foo?bar"} == Just "?bar"
  where handler :: ReaderT {uri :: String} Maybe String
        handler = dir "foo" (_.uri <$> ask) <|> lift (Just "nope")
