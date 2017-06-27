{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
-- | API definition.

module LinksAPI (API, ServiceAPI) where

import           Data.Swagger
import           LinksData
import           Servant

type API = ServiceAPI :<|> SwaggerAPI

type ServiceAPI = AddLinkEP :<|> VoteLinkEP :<|> GetLinksEP

-- | Add a link to the bookmarks.
type AddLinkEP = LinksP
              :> ReqBody '[JSON] LinkAddReq
              :> PostCreated '[JSON] LinkId

-- | Vote a link up.
type VoteLinkEP = LinksP
               :> "vote"
               :> ReqBody '[JSON] Vote
               :> Post '[JSON] NoContent

-- | Get links.
type GetLinksEP = LinksP
               :> QueryParam "sortBy" LinksSortCriterion
               :> QueryParam "first" Integer
               :> QueryParam "howMany" Integer
               :> Get '[JSON] [LinkDetails]

type LinksP = "links"

-- * Swagger API's

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger
