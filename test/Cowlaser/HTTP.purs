module Test.Cowlaser.HTTP (main) where

import Cowlaser.HTTP (headerValues)
import Data.List (List(..))
import Data.String.CaseInsensitive (CI(..))
import Prelude
import Test.Assert (assert)

main = do
  assert $ headerValues (CI "content-type")
                        {headers: Nil}
             == Nil

  assert $ headerValues (CI "content-type")
                        {headers: Cons {name: CI "Content-Type", value: "text/html"} Nil}
             == Cons "text/html" Nil

  assert $ headerValues (CI "content-type")
                        {headers: Cons {name: CI "Content-Type", value: "text/html"}
                                $ Cons {name: CI "content-Type", value: "text/plain"}
                                $ Nil}
             == Cons "text/plain" (Cons "text/html" Nil)

  assert $ headerValues (CI "content-type")
                        {headers: Cons {name: CI "Host", value: "example.com"}
                                $ Cons {name: CI "Content-Type", value: "text/html"}
                                $ Nil}
             == Cons "text/html" Nil
