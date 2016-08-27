-- | Routing combinators.
-- |
-- | #### Example
-- |
-- | ```purescript
-- | index = root *> pure "Welcome to my website!"
-- | newUser = dir "users" $ dir "new" $ root *> pure "<form>…</form>"
-- |
-- | website = index <|> newUser
-- | ```
module Cowlaser.Route
( withRouting
, method
, dir
, dirP
, root
) where

import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.Reader.Class (ask, local, class MonadReader)
import Control.MonadZero (guard, class MonadZero)
import Cowlaser.HTTP (Response)
import Data.List (List(..))
import Data.Maybe (fromMaybe)
import Node.Encoding (Encoding(UTF8))
import Node.Stream.Aff as Stream
import Prelude

-- | Add routing to the stack.
withRouting
  :: forall m eff
   . (Functor m)
  => (MaybeT m (Response eff))
  -> m (Response eff)
withRouting action = runMaybeT action <#> fromMaybe notFound
  where notFound :: forall eff. Response eff
        notFound =
          { status: {code: 404, message: "Not Found"}
          , headers: Nil
          , body: \w -> do
              Stream.writeString w UTF8 "404 Not Found"
              Stream.end w
          }

-- | Continue only if the request method matches the given string.
-- | The request method is checked case-insensitively as per the HTTP
-- | specification.
method :: forall r m
        . (MonadReader {method :: String | r} m, MonadZero m)
       => String
       -> m Unit
method lookfor =
  guard =<< (compareCI lookfor) <<< _.method <$> ask

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

-- | Continue only if the URI has no path components.
root :: forall r m
      . (MonadReader {uri :: String | r} m, MonadZero m)
     => m Unit
root = guard =<< isRoot <<< _.uri <$> ask

foreign import compareCI :: String -> String -> Boolean

foreign import extractFirstPathComponent
  :: String -> {first :: String, rest :: String}

foreign import isRoot :: String -> Boolean
