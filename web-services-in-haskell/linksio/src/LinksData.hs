{-# LANGUAGE DataKinds       #-}
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
import           Data.Time.Clock

data Link = Link
  { linkDesc :: String
  , linkUrl  :: String
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Link)

newtype UserId = UserId Integer deriving (Eq, Show)
$(deriveJSON defaultOptions ''UserId)

data LinkAddReq = LinkAddReq
  { userId    :: UserId
  , linkToAdd :: Link
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''LinkAddReq)

newtype LinkId = LinkId Integer deriving (Eq, Show)
$(deriveJSON defaultOptions ''LinkId)

data Vote = Vote
  { linkId     :: LinkId
  , voteUserId :: UserId
  , voteUp     :: Bool -- ^ True for a vote up. False for a vote down.
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Vote)

data LinksSortCriterion = DateAsc | DateDesc | RatingAsc | RatingDesc
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''LinksSortCriterion)

data LinkDetails = LinkDetails
  { addedBy     :: UserId
  , link        :: Link
  , linkVotes   :: Integer
  , linkAddedOn :: UTCTime
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''LinkDetails)

