module Test.Main (main) where

import Test.Cowlaser.Params as Test.Cowlaser.Params
import Test.Cowlaser.Route as Test.Cowlaser.Route
import Prelude

main = do
  Test.Cowlaser.Params.main
  Test.Cowlaser.Route.main
