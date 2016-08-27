module Cowlaser.Route
( dir
, dirP
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
dir lookfor = dirP (_ == lookfor)

-- | Like `dir`, but with a custom predicate. The predicate takes the first
-- | path component as its argument.
dirP :: forall r m a
      . (MonadReader {uri :: String | r} m, Plus m)
     => (String -> Boolean)
     -> m a
     -> m a
dirP pred action = do
  {first, rest} <- extractFirstPathComponent <<< _.uri <$> ask
  if pred first
    then local (_ {uri = rest}) action
    else empty

foreign import extractFirstPathComponent
  :: String -> {first :: String, rest :: String}
