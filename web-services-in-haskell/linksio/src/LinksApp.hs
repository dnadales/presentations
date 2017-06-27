module LinksApp (startApp) where

import           LinksAPI
import           LinksDocs
import           LinksServer
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = simpleCors $ serve api (linksServer mkServerEnv :<|> return swaggerDocs)

api :: Proxy API
api = Proxy
