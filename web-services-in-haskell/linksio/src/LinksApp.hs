module LinksApp (startApp) where

import           Database.Persist.Postgresql (runSqlPool)
import           LinksAPI
import           LinksDocs
import           LinksServer
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant

startApp :: IO ()
startApp = do
  env <- mkServerEnv
  runSqlPool doMigrations (getPool env)
  run 8080 (app env)

app :: ServerEnv -> Application
app env = simpleCors $ serve api (linksServer env :<|> return swaggerDocs)

api :: Proxy API
api = Proxy
