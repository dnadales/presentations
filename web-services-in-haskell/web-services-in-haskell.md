% Developing web services in Haskell
% Damian Nadales
% March 28, 2017

# Introduction

## Goal

- Show a possible way to use haskell for developing web-services
- What I'll show:
    - Set things up.
    - Generate documentation.
    - Pass context around (Database connections).
    - Persist data.
    - Containerization.

## Haskell for web applications

There are plenty of options:

- Snapp
- Yesod
- Servant

## Servant 

- Servant is focused on declaring web API's.
- API specifications are used to:
    - Write servers.
    - Obtain client functions.
    - Generate documentation.
    - And more.

# The Project 

## Description

Build a simple list application where:

- Users can bookmark links.
- Users can tag links.
- User can vote on links.

## Getting started

```sh
stack new linksio servant
cd linksio
stack build
stack exec linksio-exe
```

## We have a web-server

- Browse to `localhost:8080/users`. 
- Nice, now let's add some functionality.

# Adding the first functionality

## Anatomy of this Servant (web) server

- `src/LinksAPI.hs`: types of the API.
- `src/LinksData.hs`: data we use in our server.
- `src/LinksServer.hs`: implementation of the API server.
- `src/LinksApp.hs`: definition of the application that can be run.

## Defining an end-point

```haskell
type GetLinksEP = LinksP
               :> QueryParam "sortBy" LinksSortCriterion
               :> QueryParam "first" Integer
               :> QueryParam "howMany" Integer
               :> Get '[JSON] [Link]

type LinksP = "links"
```

## Defining the data

```haskell
data LinksSortCriterion = DateAsc | DateDesc | RatingAsc | RatingDesc
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''LinksSortCriterion)

data Link = LinkDetails
  { linkDescription :: String
  , linUrl          :: String
  , linkCreatedBy   :: UserId
  , linkCreated     :: UTCTime
  , linkVotes       :: Integer
  } deriving (Eq, Show, Generic)
```

## Implementing the end-points

```haskell
getLinks :: Maybe LinksSortCriterion
         -> Maybe Integer
         -> Maybe Integer
         -> LinksHandler [LinkDetails]
getLinks = undefined
```

## Generating documentation

```haskell
app :: Application
app = simpleCors $ serve api (linksServer mkServerEnv :<|> return swaggerDocs)

swaggerDocs :: Swagger
swaggerDocs = toSwagger serviceAPI
  where serviceAPI :: Proxy ServiceAPI
        serviceAPI = Proxy
```

```sh
docker pull swaggerapi/swagger-ui
docker run -p 80:8080 -d --rm swaggerapi/swagger-ui
```

# Adding persistence

## Getting a connection pool

```haskell
mkServerEnv :: IO ServerEnv
mkServerEnv = ServerEnv <$> devPool
  where devPool = runStdoutLoggingT (createPostgresqlPool connStr nrConn)
        connStr = "host=localhost dbname=linksdb user=test password=test port=5432"
        nrConn = 2
```

## Passing the connection pool to the main app

```haskell
newtype LinksHandler a = LinksHandler
  { runLinksHandler :: ReaderT ServerEnv (ExceptT ServantErr IO) a
  }

data ServerEnv = ServerEnv
  { getPool :: ConnectionPool }
  
startApp :: IO ()
startApp = do
  env <- mkServerEnv
  run 8080 (app env)
```

## Implementing link addition

```haskell
runDb :: (MonadReader ServerEnv m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

addLink :: LinkAddReq -> LinksHandler LinkId
addLink req =
  runDb $ insert $ (linkToAdd req)
```

# Dockerization

## Configuring stack

```yaml
docker:
    enable: false
    auto-pull: false

image:
  container:
    name: linksio
    base: fpco/stack-run
```

## Pulling, building, and running

```sh
stack docker pull
# If using sudo: sudo stack docker pull --allow-different-user
stack build --docker
# If using sudo: sudo stack build --docker --allow-different-user
stack image container --docker
# If using sudo: sudo stack image container --docker --allow-different-user
docker run -it -p 8080:8080 linksio /usr/local/bin/linksio-exe
```

Might take a while the first time as it has to download and build all the
dependencies.

# Epilogue

## Other features I did not show

- Authentication in Servant.
- Defining clients for web services (in Haskell and type-safe).


## Further reading

- [Servant](http://haskell-servant.readthedocs.io/)
- [Haskell Persistent](https://www.yesodweb.com/book/persistent)
- [Combining Haskell and Persistent](http://www.parsonsmatt.org/2016/07/08/servant-persistent_updated.html)
- [Yesod](https://www.yesodweb.com/)

