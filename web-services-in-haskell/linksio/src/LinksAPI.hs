{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
-- | API definition.

module LinksAPI (API, ServiceAPI) where

import           LinksData
import           Servant

type API = ServiceAPI

type ServiceAPI = AddLinkEP :<|> VoteLinkEP :<|> GetLinksEP

-- | Add a link to the bookmarks.
type AddLinkEP = LinksP
              :> ReqBody '[JSON] LinkAddReq
              :> PostCreated '[JSON] LinkId

-- | Vote a link up.
type VoteLinkEP = LinksP
               :> ReqBody '[JSON] Vote
               :> Post '[JSON] NoContent

-- | Get links.
type GetLinksEP = LinksP
               :> QueryParam "sortBy" LinksSortCriterion
               :> QueryParam "first" Integer
               :> QueryParam "howMany" Integer
               :> Get '[JSON] [LinkDetails]

type LinksP = "links"
