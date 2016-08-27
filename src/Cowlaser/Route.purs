module Cowlaser.Route
( dir
, dirP
, root
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

-- | Run the supplied computation if the URI has no path components.
root :: forall r m a
      . (MonadReader {uri :: String | r} m, Plus m)
     => m a
     -> m a
root action = ask <#> _.uri >>> isRoot >>= if _ then action else empty

foreign import extractFirstPathComponent
  :: String -> {first :: String, rest :: String}

foreign import isRoot :: String -> Boolean
