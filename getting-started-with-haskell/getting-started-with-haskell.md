% Getting started with Haskell
% Damian Nadales
% March 30, 2017

# Introduction

## Objectives

- Introduce Haskell's main features.
- Get you started with Haskell tooling.
- Write some Haskell!

## Prerequisites

Stack is installed in your system.

# Our first Haskell project

## Stack

- Package manager.
- Build tool.
- Scaffolding tool.
- "One-stop shop for all the Haskell tooling you need"
- Aims at achieving reproducible builds

## Creating our first project
WARNING! This needs to be done before-hand by the participants, otherwise it
will take a lot of time to download and install the ghc!

```sh
stack new hello-haskell
```

## Installing GHC through Stack

```sh
stack setup
```

## Building the project

```sh
stack build
```

## Executing the program we just build
```sh
stack exec hello-haskell-exe
```

## Anatomy of a Haskell application

TODO

## Adding additional dependencies

TODO: show how to add containers (required by Data.Map)

# A first taste of Haskell

## Let's fire up the REPL

```sh
stack ghci
```

## Simple expressions

- You can type more math expressions directly into the REPL.

## Declaring a function

## Associativity

- Function declaration associates right.
```haskell
a -> b -> c = a -> (b -> c)
```

- Function application associates left.
```haskell
f x y = (f x) y
```

## Infix notation

## Maybe

- `fromMaybe`

## Lists

```haskell
xs = [0, 1, 2]
ys = "hello"
oddNumbers = [ 2 * i + 1 | i <- [0..]]
pairs = [('a', 1), ('z', 10)]
```
## Zip

## More functions on lists

- See `Data.List`
- Example `sum`

## Map

- `fromList`
- `lookUp`

## Partial application

```haskell 
map (*2) xs
```

## Collections

Module `Data.Map` requires `collections` package.
```haskell
import Data.Map
```

# First exercise 

## Install and Setup exercism

- TODO: ask people to do this beforehand.

## Fetch the exercise

```sh
exercism fetch haskell scrabble-score
```

... and build it.

## Tip: Hoogle

Example: search for functions with type `a -> [a]`.

## Go!
- Hint: you can use functions in `Data.Char`.
- Submitting: 
```sh
exercism submit src/Scrabble.hs package.yaml
```
