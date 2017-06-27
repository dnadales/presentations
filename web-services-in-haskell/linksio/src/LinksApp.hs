module LinksApp (startApp) where

import           LinksAPI
import           LinksServer
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api (linksServer mkServerEnv)

api :: Proxy ServiceAPI
api = Proxy
