{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module LinksData
  ( Link (..)
  , UserAddReq (..)
  , LinkAddReq (..)
  , UserId
  , LinkId
  , Vote (..)
  , LinksSortCriterion
  , migrateAll
  , User (..)
  , linkVotesField
  ) where

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
User json
  name String
  email String
  created UTCTime
  UniqueEmail email
  deriving Eq Show Generic

Link json
  description String
  url String
  createdBy UserId
  created UTCTime
  votes Int
  UniqueUrl url
  deriving Eq Show Generic
|]

linkVotesField = LinkVotes

-- $(deriveJSON defaultOptions ''Link)

instance ToSchema Link

instance ToSchema (Key a) where
  declareNamedSchema _ = return (NamedSchema Nothing mempty)

data UserAddReq = UserAddReq
  { newUserName  :: String
  , newUserEmail :: String
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''UserAddReq)

instance ToSchema UserAddReq

data LinkAddReq = LinkAddReq
  { creatorId          :: UserId
  , newLinkDescription :: String
  , newLinkUrl         :: String
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

