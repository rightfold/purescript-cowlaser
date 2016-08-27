module Cowlaser.Route
( dir
) where

import Control.Monad.Reader.Class (ask, local, class MonadReader)
import Control.Plus (empty, class Plus)
import Prelude

-- | Check the first path component and run the supplied computation if it
-- | matches. The supplied computation will observe the URI without this path
-- | component.
-- |
-- | The path component must not contain a forward slash.
dir :: forall r m a
     . (MonadReader {uri :: String | r} m, Plus m)
    => String
    -> m a
    -> m a
dir lookfor action = do
  {first, rest} <- extractFirstPathComponent <<< _.uri <$> ask
  if first == lookfor
    then local (_ {uri = rest}) action
    else empty

foreign import extractFirstPathComponent
  :: String -> {first :: String, rest :: String}
