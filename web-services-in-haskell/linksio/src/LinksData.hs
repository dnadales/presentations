{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module LinksData
  ( Link
  , LinkAddReq
  , UserId
  , LinkId
  , Vote
  , LinkSortCriterion
  , LinkDetails
  ) where

import           Data.Aeson
import           Data.Aeson.TH

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
  , voteUserId :: Integer
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Vote)

data LinkSortCriterion = DateAsc | DateDesc | RatingAsc | RatingDesc
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''LinkSortCriterion)

data LinkDetails = LinkDetails
  { addedBy :: UserId
  , link    :: Link
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''LinkDetails)

