% Developing web services in Haskell
% Damian Nadales
% March 28, 2017

# Introduction

## Goals

- What to show...
    - Setting things up
    - Passing context around (Database connections).
    - Using other web services.
    - Generating documentation.
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
               :> Get '[JSON] [LinkDetails]

type LinksP = "links"
```

## Defining the data

```haskell
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
