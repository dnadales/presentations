{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module LinksData
  ( Link
  , LinkAddReq
  , UserId
  , LinkId
  , Vote
  , LinksSortCriterion
  , LinkDetails
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Swagger
import           Data.Time.Clock
import           Data.Typeable
import           GHC.Generics
import           Web.HttpApiData

data Link = Link
  { linkDesc :: String
  , linkUrl  :: String
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Link)

instance ToSchema Link

newtype UserId = UserId Integer deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''UserId)

instance ToSchema UserId

data LinkAddReq = LinkAddReq
  { userId    :: UserId
  , linkToAdd :: Link
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''LinkAddReq)

instance ToSchema LinkAddReq

newtype LinkId = LinkId Integer deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''LinkId)

instance ToSchema LinkId

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
