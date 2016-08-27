-- | Routing combinators.
-- |
-- | Routes are simply higher-order request handlers that may fail using
-- | `MonadZero`.
-- |
-- | #### Example
-- |
-- | ```purescript
-- | index = root $ pure "Welcome to my website!"
-- | newUser = dir "users" $ dir "new" $ root $ pure "<form>…</form>"
-- |
-- | website = index <|> newUser
-- | ```
module Cowlaser.Route
( method
, dir
, dirP
, root
) where

import Control.Monad.Reader.Class (ask, local, class MonadReader)
import Control.MonadZero (guard, class MonadZero)
import Prelude

-- | Check the request method and run the supplied computation if it matches.
-- | The request method is checked case-insensitively as per the HTTP
-- | specification.
method :: forall r m a
        . (MonadReader {method :: String | r} m, MonadZero m)
       => String
       -> m a
       -> m a
method lookfor action = do
  guard =<< (compareCI lookfor) <<< _.method <$> ask
  action

-- | Check the first path component and run the supplied computation if it
-- | matches. The supplied computation will observe the URI without this path
-- | component.
-- |
-- | The path component must not contain a forward slash.
dir :: forall r m a
     . (MonadReader {uri :: String | r} m, MonadZero m)
    => String
    -> m a
    -> m a
dir lookfor = dirP (_ == lookfor)

-- | Like `dir`, but with a custom predicate. The predicate takes the first
-- | path component as its argument.
dirP :: forall r m a
      . (MonadReader {uri :: String | r} m, MonadZero m)
     => (String -> Boolean)
     -> m a
     -> m a
dirP pred action = do
  {first, rest} <- extractFirstPathComponent <<< _.uri <$> ask
  guard (pred first)
  local (_ {uri = rest}) action

-- | Run the supplied computation if the URI has no path components.
root :: forall r m a
      . (MonadReader {uri :: String | r} m, MonadZero m)
     => m a
     -> m a
root action = do
  guard =<< isRoot <<< _.uri <$> ask
  action

foreign import compareCI :: String -> String -> Boolean

foreign import extractFirstPathComponent
  :: String -> {first :: String, rest :: String}

foreign import isRoot :: String -> Boolean
