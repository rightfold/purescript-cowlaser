module Cowlaser.Params
( query
, query'
) where

import Control.Monad.Reader.Class (ask, class MonadReader)
import Data.List as List
import Data.List (List)
import Data.Maybe (Maybe)
import Prelude

-- | Read all values of a query string parameter.
query :: forall r m
       . (MonadReader {uri :: String | r} m)
      => String
      -> m (List String)
query key = List.fromFoldable <<< _query key <<< _.uri <$> ask

-- | Read the first value of a query string parameter.
query' :: forall r m
        . (MonadReader {uri :: String | r} m)
       => String
       -> m (Maybe String)
query' key = List.head <$> query key

foreign import _query :: String -> String -> Array String
