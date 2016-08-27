module Cowlaser.Serve
( nodeHandler
) where

import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Cowlaser.HTTP (Request, Response)
import Data.List (List(..))
import Data.Maybe (fromMaybe)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Node.Encoding (Encoding(UTF8))
import Node.HTTP (HTTP)
import Node.HTTP as N
import Node.Stream.Aff as Stream
import Prelude

-- | `nodeHandler` takes a Cowlaser request handler and returns a Node request
-- | handler. You can pass this handler to `Node.HTTP.createServer`.
nodeHandler
  :: forall eff
   . ReaderT (Request eff) (MaybeT (Aff (http :: HTTP | eff))) (Response eff)
  -> (N.Request -> N.Response -> Eff (http :: HTTP | eff) Unit)
nodeHandler handler nReq nRes = void $
  runAff (\_ -> pure unit)
         (\_ -> pure unit)
         (runMaybeT (runReaderT handler (node2req nReq))
            >>= fromMaybe notFound >>> res2node nRes)

notFound :: forall eff. Response eff
notFound =
  { status: {code: 404, message: "Not Found"}
  , headers: Nil
  , body: \w -> do
      Stream.writeString w UTF8 "404 Not Found"
      Stream.end w
  }

node2req :: forall eff. N.Request -> Request eff
node2req nReq =
  { method: N.requestMethod nReq
  , uri: N.requestURL nReq
  , headers: StrMap.toList (N.requestHeaders nReq)
             # map \(Tuple name value) -> {name, value}
  , body: N.requestAsStream nReq
  }

res2node :: forall r eff. N.Response -> Response eff -> Aff (http :: HTTP | eff) Unit
res2node nRes res = do
  liftEff $ do
    N.setStatusCode nRes res.status.code
    N.setStatusMessage nRes res.status.message
    -- TODO: N.setHeaders
  res.body (N.responseAsStream nRes)
