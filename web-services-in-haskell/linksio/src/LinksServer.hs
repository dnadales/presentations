{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module LinksServer
  ( linksServer
  , mkServerEnv
  , ServerEnv
  , doMigrations
  , getPool
  ) where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Database.Persist.Postgresql
import           Database.Persist.Sql
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
  } deriving (Functor, Applicative, Monad, MonadReader ServerEnv, MonadIO)

data ServerEnv = ServerEnv
  { getPool :: ConnectionPool }

mkServerEnv :: IO ServerEnv
mkServerEnv = ServerEnv <$> devPool
  where devPool = runStdoutLoggingT (createPostgresqlPool connStr nrConn)
        connStr = "host=localhost dbname=linksdb user=tester password=test port=5432"
        nrConn = 2

addLink :: LinkAddReq -> LinksHandler LinkId
addLink req =
  runDb $ insert $ (linkToAdd req)

voteLink :: Vote -> LinksHandler NoContent
voteLink = undefined

getLinks :: Maybe LinksSortCriterion
         -> Maybe Integer
         -> Maybe Integer
         -> LinksHandler [LinkDetails]
getLinks = undefined

-- * Utility functions to run db queries.
--
-- Taken from
--   http://www.parsonsmatt.org/2016/07/08/servant-persistent_updated.html

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader ServerEnv m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

