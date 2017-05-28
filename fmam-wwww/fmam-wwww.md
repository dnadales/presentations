% Functors, Applicatives, Monads, and Alternatives: What, Why, When, Where?
% Damian Nadales
% June 1, 2017
---
height: 400
...

# Introduction

## Goals

- Show how you can achieve higher levels of code reuse through the use of these
  abstractions.

## A word of warning

- Don't expect to understand monads after this presentation.
    - Knowledge is not acquired linearly
    - These are very generic abstractions
- I'm doing my best, but you should learn this from the best.

# Functors

## This story begins with the billion dollar mistake

```java
public Address getAddress(i: Int) {...}
```

## Solving the billion dollar mistake

```haskell
getAddress :: Int -> AddressBook -> Maybe Address
getAddress = Map.lookup
```

## Addresses

```haskell
newtype Address = Address { address :: String }

type AddressBook = Map Int Address

-- | Some hard-coded addresses:
addressBook = Map.fromList [ (0, Address "Foo addr. 0")
                           , (1, Address "Bar addr. 1")]
```

## Getting the digits inside an address

```haskell
getAddressDigits :: Int -> AddressBook -> Maybe String
getAddressDigits i aBook =
  case getAddress i aBook of
    Nothing             -> Nothing
    Just (Address name) -> Just (filter isDigit name)
```

## Computing the length of an address

```haskell
lengthAddress :: Int -> AddressBook -> Maybe Int
lengthAddress i aBook =
  case getAddress i aBook of
    Just (Address name) -> Just (length name)
    Nothing             -> Nothing
```

## We have some code duplication

```haskell
mapAddressBook :: (String -> b) -> Int -> AddressBook -> Maybe b
mapAddressBook f i aBook =
  case getAddress i aBook of
    Just (Address name) -> Just (f name)
    Nothing             -> Nothing

getAddressDigits' :: Int -> AddressBook -> Maybe String
getAddressDigits' i aBook = mapAddressBook (filter isDigit) i aBook

lengthAddress' :: Int -> AddressBook -> Maybe Int
lengthAddress' = mapAddressBook length
```

## Are we happy yet?

`mapAddressBook` is related to `Maybe` (more than addresses...)

```haskell
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f (Just x) = Just (f x)
mapMaybe _ Nothing  = Nothing

mapAddressBook' :: (String -> b) -> Int -> AddressBook -> Maybe b
mapAddressBook' f i aBook =
  (f . address) `mapMaybe` (getAddress i aBook)
```

## Let's extract the digits in a file

```haskell
digitsInFile :: String -> IO String
digitsInFile path = do
  contents <- readFile path
  return (filter isDigit contents)
```

## Let's compute file lenght

```haskell
lengthFile :: String -> IO Int
lengthFile path = do
  contents <- readFile path
  return (length contents)
```

## More duplication

```haskell
mapIO :: (a -> b) -> IO a -> IO b
mapIO f ioa = do
  x <- ioa
  return (f x)
  
digitsInFile' path = (filter isDigit) `mapIO` readFile path
lengthFile' path = length `mapIO` readFile path
```

## Digits in a list of strings

```haskell
digitsInList :: [String] -> [String]
digitsInList []       = []
digitsInList (ns:nss) = (filter isDigit ns):(digitsInList nss)
```

## Lengths of a list of strings

```haskell
lengthStrings :: [String] -> [Int]
lengthStrings []       = []
lengthStrings (ns:nss) = (length ns):(lengthStrings nss)
```

## Use map!

```haskell
map :: (a -> b) -> [a] -> [b]

digitsInList' = map (filter isDigit)
lengthStrings' = map length
```

## We can generalize even more

Look at all the type signatures we've been using ...

```haskell
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapIO    :: (a -> b) -> IO    a -> IO    b
map      :: (a -> b) -> []    a -> []    b
```

It seems only the type constructor is different!

## Enter functors

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

(<$>) = fmap

instance Functor Maybe
instance Functor IO
instance Functor []
```

## Reducing our code even more

We can get rid of `digitsInFile`, `lenghtFile`, `digitsInList`, and
`legnthStrings`.

```haskell
digitsInF :: Functor f => f String -> f String
digitsInF = ((filter isDigit) <$>)

lengthF :: Functor f => f [a] -> f Int
lengthF = (length <$>)
```

# Applicatives

## Parsing

TODO: Maybe $ show a parser in the slides.

```haskell
newtype Parser s a = P { runP :: s -> Maybe (a, s) }
```

# Epilogue 

## Conclusions

## Further reading

- [The monad tutorial fallacy](https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/)
