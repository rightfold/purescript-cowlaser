module Test.Cowlaser.Route (main) where

import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Cowlaser.Route (dir, dirA, method, root)
import Data.Maybe (Maybe(..))
import Data.String.CaseInsensitive (CI(..), unCI)
import Prelude
import Test.Assert (assert)

main = do
  assert $ runReaderT get {method: CI "GET"} == Just "GET"
  assert $ runReaderT get {method: CI "get"} == Just "get"
  assert $ runReaderT get {method: CI "post"} == Nothing
  assert $ runReaderT get {method: CI ""} == Nothing

  assert $ runReaderT foo {uri: ""} == Nothing
  assert $ runReaderT foo {uri: "?foo"} == Nothing
  assert $ runReaderT foo {uri: "/"} == Nothing
  assert $ runReaderT foo {uri: "/?foo"} == Nothing
  assert $ runReaderT foo {uri: "/foo"} == Just ""
  assert $ runReaderT foo {uri: "/foo/bar"} == Just "/bar"
  assert $ runReaderT foo {uri: "/foo?bar"} == Just "?bar"

  assert $ runReaderT bar {uri: ""} == Nothing
  assert $ runReaderT bar {uri: "/"} == Nothing
  assert $ runReaderT bar {uri: "/foo/bar"} == Just "foo -- /bar"

  assert $ runReaderT index {uri: ""} == Just ""
  assert $ runReaderT index {uri: "?foo"} == Just "?foo"
  assert $ runReaderT index {uri: "/"} == Just "/"
  assert $ runReaderT index {uri: "/?foo"} == Just "/?foo"
  assert $ runReaderT index {uri: "/foo"} == Nothing
  where get :: ReaderT {method :: CI} Maybe String
        get = (method (CI "get") *> (unCI <<< _.method <$> ask))

        foo :: ReaderT {uri :: String} Maybe String
        foo = dir "foo" (_.uri <$> ask)

        bar :: ReaderT {uri :: String} Maybe String
        bar = dirA \x -> (\y -> x <> " -- " <> y.uri) <$> ask

        index :: ReaderT {uri :: String} Maybe String
        index = (root *> (_.uri <$> ask))
