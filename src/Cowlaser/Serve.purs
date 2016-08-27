module Cowlaser.Serve
( nodeHandler
) where

import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Class (class MonadReader)
import Control.Monad.Reader.Trans (runReaderT)
import Cowlaser.HTTP (Request, Response)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Node.HTTP (HTTP)
import Node.HTTP as N
import Prelude

-- | `nodeHandler` takes a Cowlaser request handler and returns a Node request
-- | handler. You can pass this handler to `Node.HTTP.createServer`.
nodeHandler
  :: forall eff
   . (  forall m
      . ( MonadAff (http :: HTTP | eff) m
        , MonadReader (Request eff) m
        )
     => m (Response eff)
     )
  -> (N.Request -> N.Response -> Eff (http :: HTTP | eff) Unit)
nodeHandler handler nReq nRes = void $
  runAff (\_ -> pure unit)
         (\_ -> pure unit)
         (runReaderT handler (node2req nReq) >>= res2node nRes)

node2req :: forall eff. N.Request -> Request eff
node2req nReq =
  { method: N.requestMethod nReq
  , uri: N.requestURL nReq
  , headers: StrMap.toList (N.requestHeaders nReq)
             # map \(Tuple name value) -> {name, value}
  , body: N.requestAsStream nReq
  }

res2node :: forall eff. N.Response -> Response eff -> Aff (http :: HTTP | eff) Unit
res2node nRes res = do
  liftEff $ do
    N.setStatusCode nRes res.status.code
    N.setStatusMessage nRes res.status.message
    -- TODO: N.setHeaders
  res.body (N.responseAsStream nRes)
