{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Control.Concurrent.Async
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as L
import           Data.Char
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- * Functors

-- ** Let's start with the billion dollar mistake.

newtype Address = Address { address :: String }

type AddressBook = Map Int Address

-- | Some hard-coded addresses:
addressBook = Map.fromList [ (0, Address "Foo addr. 0")
                           , (1, Address "Bar addr. 1")]

-- > getAddress :: Int -> AddressBook -> Address -- WRONG

-- | Get a Address.
getAddress :: Int -> AddressBook -> Maybe Address
getAddress = Map.lookup

-- | Get a the digits inside an address.
getAddressDigits :: Int -> AddressBook -> Maybe String
getAddressDigits i aBook =
  case getAddress i aBook of
    Nothing             -> Nothing
    Just (Address name) -> Just (filter isDigit name)

lengthAddress :: Int -> AddressBook -> Maybe Int
lengthAddress i aBook =
  case getAddress i aBook of
    Just (Address name) -> Just (length name)
    Nothing             -> Nothing

-- It seems like we have some code duplication...
mapAddressBook :: (String -> b) -> Int -> AddressBook -> Maybe b
mapAddressBook f i aBook =
  case getAddress i aBook of
    Just (Address name) -> Just (f name)
    Nothing             -> Nothing

getAddressDigits' :: Int -> AddressBook -> Maybe String
getAddressDigits' i aBook = mapAddressBook (filter isDigit) i aBook

lengthAddress' :: Int -> AddressBook -> Maybe Int
lengthAddress' = mapAddressBook length

-- ** Let's more abstract!
--
--  If you think about it, @mapAddressBook@ is implementing a function that is
--  more related to the @Maybe@ type.

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f (Just x) = Just (f x)
mapMaybe _ Nothing  = Nothing

mapAddressBook' :: (String -> b) -> Int -> AddressBook -> Maybe b
mapAddressBook' f i aBook =
  (f . address) `mapMaybe` (getAddress i aBook)

getAddressDigits'' :: Int -> AddressBook -> Maybe String
getAddressDigits'' = mapAddressBook' (filter isDigit)

lengthAddress'' :: Int -> AddressBook -> Maybe Int
lengthAddress'' = mapAddressBook length

-- So we got an (amortized) 3 times code size reduction.

-- ** Let's cause side effects.

-- | Extract the digits in a file.
digitsInFile :: String -> IO String
digitsInFile path = do
  contents <- readFile path
  return (filter isDigit contents)

lengthFile :: String -> IO Int
lengthFile path = do
  contents <- readFile path
  return (length contents)

mapIO :: (a -> b) -> IO a -> IO b
mapIO f ioa = do
  x <- ioa
  return (f x)

-- | @upperFile@ using @fmap@
digitsInFile' path = (filter isDigit) `mapIO` readFile path
lengthFile' path = length `mapIO` readFile path

-- ** The good old lists.
-- | Get the digits in a list of strings.
digitsInList :: [String] -> [String]
digitsInList []       = []
digitsInList (ns:nss) = (filter isDigit ns):(digitsInList nss)

lengthStrings :: [String] -> [Int]
lengthStrings []       = []
lengthStrings (ns:nss) = (length ns):(lengthStrings nss)

-- But we have:
--
-- > map :: (a -> b) -> [a] -> [b]

digitsInList' = map (filter isDigit)

lengthStrings' :: [String] -> [Int]
-- WARNING: the type of this function is more generic as it can operate over
-- any foldable.
lengthStrings' = map length

-- ** Using @fmap@'s

-- But let's look at all the signatures we've been using:
--
-- > mapMaybe :: (a -> b) -> Maybe a -> Maybe b
-- > mapIO    :: (a -> b) -> IO    a -> IO    b
-- > map      :: (a -> b) -> []    a -> []    b

-- We could use @fmap@ from functors! Then we don't need @mapMaybe@, @mapIO@, and @map@!

-- Furthermore, we can get rid of the @digitsInFile@, @digitsInIO@,
-- @lengthFile@ and @lengthStrings@!
digitsInF :: Functor f => f String -> f String
digitsInF = ((filter isDigit) <$>)

lengthF :: Functor f => f [a] -> f Int
lengthF = (length <$>)

-- The key is: getting rid of the accidental complexity through higher order.

-- * Applicatives

newtype Name = Name { name :: String }

data Employee = Employee Name Address

type NameDir = Map Int Name

getName = Map.lookup

getEmployee :: Int -> NameDir -> AddressBook -> Maybe Employee
getEmployee i nDir aBook =
  case getName i nDir of
    Nothing -> Nothing
    Just name ->
      case getAddress i aBook of
        Nothing      -> Nothing
        Just address -> Just (Employee name address)

-- Imagine that we'll have to add a third field to employee...
--
-- Twister!

-- What if we want to read an employee from the console
readEmployee :: IO Employee
readEmployee = do
  name <- getLine
  address <- getLine
  return (Employee (Name name) (Address address))

-- If you look at the code above, we're extracting the values inside the
-- Maybe's (if any) and IO's, and applying the @Employee@ constructor.

-- ** Two functions to fight the twister:
-- pure  :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

getEmployee' i nDir aBook =
  pure Employee <*> getName i nDir <*> getAddress i aBook

readEmployee' =
  pure Employee <*> (Name <$> getLine) <*> (Address <$> getLine)

