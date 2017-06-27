-- | Definition of the documentation server and related functionality.

module LinksDocs (swaggerDocs) where

import           Data.Swagger
import           LinksAPI
import           Servant
import           Servant.Swagger

swaggerDocs :: Swagger
swaggerDocs = toSwagger serviceAPI
  where serviceAPI :: Proxy ServiceAPI
        serviceAPI = Proxy
