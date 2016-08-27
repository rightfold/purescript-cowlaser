module Test.Cowlaser.Route (main) where

import Control.Alt ((<|>))
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Cowlaser.Route (dir, method, root)
import Data.Maybe (Maybe(..))
import Data.String.CaseInsensitive (CI(..), unCI)
import Prelude
import Test.Assert (assert)

main = do
  assert $ runReaderT get {method: CI "GET"} == Just "GET"
  assert $ runReaderT get {method: CI "get"} == Just "get"
  assert $ runReaderT get {method: CI "post"} == Just "nope"
  assert $ runReaderT get {method: CI ""} == Just "nope"

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
  where get :: ReaderT {method :: CI} Maybe String
        get = (method (CI "get") *> (unCI <<< _.method <$> ask)) <|> lift (Just "nope")

        foo :: ReaderT {uri :: String} Maybe String
        foo = dir "foo" (_.uri <$> ask) <|> lift (Just "nope")

        index :: ReaderT {uri :: String} Maybe String
        index = (root *> (_.uri <$> ask)) <|> lift (Just "nope")
