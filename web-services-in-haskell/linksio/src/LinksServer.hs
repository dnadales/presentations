{-# LANGUAGE TypeOperators #-}
module LinksServer
  ( linksServer
  , mkServerEnv
  ) where

import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           LinksAPI
import           LinksData
import           Servant

linksServer :: ServerEnv -> Server ServiceAPI
linksServer env = enter (linksHandlerNT env) linksServerT

-- | Natural transformation from out custom handler monad to the one provided
-- by Servant.
linksHandlerNT ::ServerEnv -> LinksHandler :~> Handler
linksHandlerNT env = Nat $ runHandler env
  where runHandler env act = runReaderT (runLinksHandler act) env

linksServerT :: ServerT ServiceAPI LinksHandler
linksServerT = addLink
         :<|> voteLink
         :<|> getLinks

-- | Custom monad that allows to pass an environment around.
newtype LinksHandler a = LinksHandler
  { runLinksHandler :: ReaderT ServerEnv (ExceptT ServantErr IO) a
  }

data ServerEnv = ServerEnv

mkServerEnv :: ServerEnv
mkServerEnv = ServerEnv

addLink :: LinkAddReq -> LinksHandler LinkId
addLink = undefined

voteLink :: Vote -> LinksHandler NoContent
voteLink = undefined

getLinks :: Maybe LinksSortCriterion
         -> Maybe Integer
         -> Maybe Integer
         -> LinksHandler [LinkDetails]
getLinks = undefined

