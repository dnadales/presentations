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

## Anatomy of a Servant (web) server

- `src/LinksAPI.hs`: types of the API.
- `src/LinksData.hs`: data we use in our server.
