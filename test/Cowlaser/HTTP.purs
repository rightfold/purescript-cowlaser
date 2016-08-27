module Test.Cowlaser.HTTP (main) where

import Cowlaser.HTTP (headerValues)
import Data.List (List(..))
import Prelude
import Test.Assert (assert)

main = do
  assert $ headerValues "content-type"
                        {headers: Nil}
             == Nil

  assert $ headerValues "content-type"
                        {headers: Cons {name: "Content-Type", value: "text/html"} Nil}
             == Cons "text/html" Nil

  assert $ headerValues "content-type"
                        {headers: Cons {name: "Content-Type", value: "text/html"}
                                $ Cons {name: "content-Type", value: "text/plain"}
                                $ Nil}
             == Cons "text/plain" (Cons "text/html" Nil)

  assert $ headerValues "content-type"
                        {headers: Cons {name: "Host", value: "example.com"}
                                $ Cons {name: "Content-Type", value: "text/html"}
                                $ Nil}
             == Cons "text/html" Nil
