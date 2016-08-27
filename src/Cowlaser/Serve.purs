module Cowlaser.Serve
( nodeHandler
) where

import Control.Monad.Aff (Aff, makeAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Cowlaser.HTTP (Request, Response)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Node.HTTP (HTTP)
import Node.HTTP as N
import Node.Stream as Stream
import Prelude

-- | `nodeHandler` takes a Cowlaser request handler and returns a Node request
-- | handler.
nodeHandler
  :: forall eff
   . ReaderT (Request eff) (MaybeT (Aff (http :: HTTP | eff))) (Response eff)
  -> (N.Request -> N.Response -> Eff (http :: HTTP | eff) Unit)
nodeHandler handler nReq nRes =
  let req = node2req nReq
    in runAff (\_ -> pure unit)
              (\_ -> pure unit)
              (runMaybeT (runReaderT handler req)
               >>= case _ of
                     Just res -> res2node nRes res
                     Nothing  -> res2node nRes notFound)
       # void

notFound :: forall eff. Response eff
notFound = { status: {code: 404, message: "Not Found"}
           , headers: Nil
           , body: \w -> makeAff \_ r -> Stream.end w (r unit)
           }

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
