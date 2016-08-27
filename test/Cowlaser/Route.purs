module Test.Cowlaser.Route (main) where

import Control.Alt ((<|>))
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Cowlaser.Route (dir, root)
import Data.Maybe (Maybe(..))
import Prelude
import Test.Assert (assert)

main = do
  assert $ runReaderT foo {uri: ""} == Just "nope"
  assert $ runReaderT foo {uri: "?foo"} == Just "nope"
  assert $ runReaderT foo {uri: "/"} == Just "nope"
  assert $ runReaderT foo {uri: "/?foo"} == Just "nope"
  assert $ runReaderT foo {uri: "/foo"} == Just ""
  assert $ runReaderT foo {uri: "/foo/bar"} == Just "/bar"
  assert $ runReaderT foo {uri: "/foo?bar"} == Just "?bar"

  assert $ runReaderT index {uri: ""} == Just ""
  assert $ runReaderT index {uri: "?foo"} == Just "?foo"
  assert $ runReaderT index {uri: "/"} == Just "/"
  assert $ runReaderT index {uri: "/?foo"} == Just "/?foo"
  assert $ runReaderT index {uri: "/foo"} == Just "nope"
  where foo :: ReaderT {uri :: String} Maybe String
        foo = dir "foo" (_.uri <$> ask) <|> lift (Just "nope")

        index :: ReaderT {uri :: String} Maybe String
        index = root (_.uri <$> ask) <|> lift (Just "nope")
