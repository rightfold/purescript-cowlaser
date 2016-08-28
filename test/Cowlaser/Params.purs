module Test.Cowlaser.Params (main) where

import Control.Monad.Reader (ask, Reader, runReader)
import Cowlaser.Params (query, query')
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Prelude
import Test.Assert (assert)

main = do
  assert $ runReader x {uri: ""} == Nil
  assert $ runReader x' {uri: ""} == Nothing

  assert $ runReader x {uri: "?x=a"} == Cons "a" Nil
  assert $ runReader x' {uri: "?x=a"} == Just "a"

  assert $ runReader x {uri: "?x=a&y=b"} == Cons "a" Nil
  assert $ runReader x' {uri: "?x=a&y=b"} == Just "a"

  assert $ runReader x {uri: "?x=a&x=b"} == Cons "a" (Cons "b" Nil)
  assert $ runReader x' {uri: "?x=a&x=b"} == Just "a"
  where x :: Reader {uri :: String} (List String)
        x = query "x"

        x' :: Reader {uri :: String} (Maybe String)
        x' = query' "x"
