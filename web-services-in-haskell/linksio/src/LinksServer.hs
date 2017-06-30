{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
module LinksServer
  ( linksServer
  , mkServerEnv
  , ServerEnv
  , doMigrations
  , getPool
  ) where

import           Control.Exception           (IOException, SomeException, throw,
                                              try)
import           Control.Monad.Error.Class   (MonadError, catchError,
                                              throwError)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.Time.Clock
import           Database.Persist.Class      (PersistUniqueRead)
import           Database.Persist.Postgresql
import           Database.Persist.Sql
import           LinksAPI
import           LinksData                   hiding (Link)
import qualified LinksData                   as LD (Link (..))
import           Servant

linksServer :: ServerEnv -> Server ServiceAPI
linksServer env = enter (linksHandlerNT env) linksServerT

-- | Natural transformation from out custom handler monad to the one provided
-- by Servant.
linksHandlerNT ::ServerEnv -> LinksHandler :~> Handler
linksHandlerNT env = Nat $ runHandler env
  where runHandler env act = runReaderT (runLinksHandler act) env

linksServerT :: ServerT ServiceAPI LinksHandler
linksServerT = addUser
         :<|> addLink
         :<|> voteLink
         :<|> getLinks

-- | Custom monad that allows to pass an environment around.
newtype LinksHandler a = LinksHandler
  { runLinksHandler :: ReaderT ServerEnv (ExceptT ServantErr IO) a
  } deriving (Functor, Applicative, Monad, MonadReader ServerEnv, MonadIO, MonadError ServantErr)

data ServerEnv = ServerEnv
  { getPool :: ConnectionPool }

mkServerEnv :: IO ServerEnv
mkServerEnv = ServerEnv <$> devPool
  where devPool = runStdoutLoggingT (createPostgresqlPool connStr nrConn)
        connStr = "host=linksio-db dbname=linksdb user=tester password=test port=5432"
        nrConn = 3

addUser :: UserAddReq -> LinksHandler UserId
addUser req = runDb $ do
  currTime <- liftIO $ getCurrentTime
  insert $ User (newUserName req) (newUserEmail req) currTime

addLink :: LinkAddReq -> LinksHandler LinkId
addLink req = runDb $ do
  getE (creatorId req)
  currTime <- liftIO $ getCurrentTime
  insert $ LD.Link (newLinkDescription req) (newLinkUrl req) (creatorId req) currTime 0

voteLink :: Vote -> LinksHandler NoContent
voteLink vote = runDb $ do
  update (linkId vote) [linkVotesField +=. c]
  return NoContent
  where c = if (voteUp vote) then 1 else -1

getLinks :: Maybe LinksSortCriterion
         -> Maybe Integer
         -> Maybe Integer
         -> LinksHandler [LD.Link]
getLinks _ _ _ = (map entityVal) <$> runDb (selectList [] [])

-- * Utility functions to run db queries.
--
-- Taken from PersistEntity val => Key val -> m (Maybe val)
--   http://www.parsonsmatt.org/2016/07/08/servant-persistent_updated.html

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader ServerEnv m, MonadIO m, MonadError ServantErr m)
      => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks getPool
  res <- liftIO $ try (runSqlPool query pool)
  case res of
    Left (e :: ServantErr) -> throwError e
    Right res              -> return res

-- | Try to get an element from the database by id. Fail with a Servant 404
-- error if not found.
getE :: ( MonadIO m
        , PersistRecordBackend record backend
        , PersistStoreRead backend
        ) => Key record -> ReaderT backend m record
getE k = do
  mval <- get k
  case mval of
    Nothing  -> throw err404
    Just val -> return val
