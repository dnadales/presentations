{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module LinksData
  ( Link
  , LinkAddReq
  , UserId
  , LinkId
  , Vote
  , LinksSortCriterion
  , LinkDetails
  ) where

import           Control.Lens
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Swagger
import           Data.Time.Clock
import           Data.Typeable
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           GHC.Generics
import           Web.HttpApiData

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Link
    description String
    url String
    deriving Eq Show Generic
|]

-- data Link = Link
--   { linkDesc :: String
--   , linkUrl  :: String
--   } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Link)

instance ToSchema Link

instance ToSchema (Key a) where
  declareNamedSchema _ = return (NamedSchema Nothing mempty)

newtype UserId = UserId Integer deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''UserId)

instance ToSchema UserId

data LinkAddReq = LinkAddReq
  { userId    :: UserId
  , linkToAdd :: Link
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''LinkAddReq)

instance ToSchema LinkAddReq

data Vote = Vote
  { linkId     :: LinkId
  , voteUserId :: UserId
  , voteUp     :: Bool -- ^ True for a vote up. False for a vote down.
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Vote)

instance ToSchema Vote

data LinksSortCriterion = DateAsc | DateDesc | RatingAsc | RatingDesc
  deriving (Eq, Show, Bounded, Enum, Generic, Typeable)
$(deriveJSON defaultOptions ''LinksSortCriterion)

instance ToSchema LinksSortCriterion

instance ToParamSchema LinksSortCriterion

instance FromHttpApiData LinksSortCriterion where
  parseUrlPiece = parseBoundedTextData

data LinkDetails = LinkDetails
  { addedBy     :: UserId
  , link        :: Link
  , linkVotes   :: Integer
  , linkAddedOn :: UTCTime
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''LinkDetails)

instance ToSchema LinkDetails
