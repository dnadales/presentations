{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
-- | API definition.

module LinksAPI (API) where

import Servant
import LinksData

type API = AddLinkEP

-- | Add a link to the bookmarks.
type AddLinkEP = LinksP
              :> ReqBody '[JSON] LinkAddReq
              :> PostCreated '[JSON] LinkId

-- | Vote a link up.
type VoteLinkEP = LinksP :> ReqBody '[JSON] Vote :> Post '[JSON] NoContent

-- | Get links.
type GetLinksEP = LinksP
               :> QueryParam "sortBy" LinkSortCriterion
               :> QueryParam "first" Integer
               :> QueryParam "howMany" Integer
               :> Get '[JSON] [LinkDetails]

type LinksP = "links"
