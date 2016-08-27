module Test.Main (main) where

import Test.Cowlaser.HTTP as Test.Cowlaser.HTTP
import Test.Cowlaser.Route as Test.Cowlaser.Route
import Prelude

main = do
  Test.Cowlaser.HTTP.main
  Test.Cowlaser.Route.main
