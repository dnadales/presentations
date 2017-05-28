{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Control.Applicative
import           Control.Concurrent.Async
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as L
import           Data.Char
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           GHC.IO
import           System.IO

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
    Nothing             -> Nothing
    Just (Address name) -> Just (length name)

-- It seems like we have some code duplication...
mapAddressBook :: (String -> b) -> Int -> AddressBook -> Maybe b
mapAddressBook f i aBook =
  case getAddress i aBook of
    Just (Address name) -> Just (f name)
    Nothing             -> Nothing

getAddressDigits' :: Int -> AddressBook -> Maybe String
getAddressDigits' = mapAddressBook (filter isDigit)

lengthAddress' :: Int -> AddressBook -> Maybe Int
lengthAddress' = mapAddressBook length

-- ** Let's get more abstract!
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

-- * Monads

-- Imagine we'd have to lookup the employee's id by her social security number:
type SSNDir = Map Int Int

getId = Map.lookup

getEmployeeBySSD :: Int -> SSNDir -> NameDir -> AddressBook -> Maybe Employee
getEmployeeBySSD ssn sDir nDir aBook =
  -- We'd like to use:
--
-- > pure Employee <*> getName i nDir <*> getAddress i aBook
--
-- But @i@ has to come from the lookup operation @getId@, which can return nothing!
--
-- We can't simply complete the code above by adding:
--
-- > where i = getId ssn
  case getId ssn sDir of
    Nothing -> Nothing
    Just i  -> pure Employee <*> getName i nDir <*> getAddress i aBook

-- Good, but it seems there is some boilerplate left.

-- If we'd had some function:
flatMapMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
flatMapMaybe ma fmb =
  case ma of
    Nothing -> Nothing
    Just x  -> fmb x

-- Then we could re-write:
getEmployeeBySSD' ssn sDir nDir aBook =
  getId ssn sDir `flatMapMaybe`
  \i -> pure Employee <*> getName i nDir <*> getAddress i aBook

-- ** Permutations of a list

-- Write a function that returns all the permutations of a list:

-- We'll use an auxiliary function:
--
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = [x:y:ys] ++ map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) =
  -- This won't compile
  --
  -- > map (interleave x) (perms xs)
  --
  flatMapList (perms xs) (interleave x)

flatMapList :: [a] -> (a -> [b]) -> [b]
flatMapList xs f = concat (map f xs)

-- Re-writing our monadic examples
getEmployeeBySSD'' ssn sDir nDir aBook =
  getId ssn sDir >>=
  \i -> pure Employee <*> getName i nDir <*> getAddress i aBook

perms' []     = [[]]
perms' (x:xs) = perms xs >>= interleave x

-- ** Do notation
tripleGetLine :: IO (String, String, String)
tripleGetLine =
  getLine >>= \l0 ->
  getLine >>= \l1 ->
  getLine >>= \l2 ->
  return (l0, l1, l2)

tripleGetLine' = do
  l0 <- getLine
  l1 <- getLine
  l2 <- getLine
  return (l0, l1, l2)

-- Re-writing our examples with do notation:
getEmployeeBySSD''' ssn sDir nDir aBook = do
  i <- getId ssn sDir
  pure Employee <*> getName i nDir <*> getAddress i aBook

-- Or even...

getEmployeeBySSD'''' ssn sDir nDir aBook = do
  i <- getId ssn sDir
  name <- getName i nDir
  addr <- getAddress i aBook
  return (Employee name addr)

perms'' []     = [[]]
perms'' (x:xs) = do
  ys <- perms xs
  return (interleave x ys)

-- * Alternative

-- What if we didn't know where data come from?
findEmployee :: Int -> Map Int Employee -> Map Int Employee -> Maybe Employee
findEmployee i dir0 dir1 =
  case Map.lookup i dir0 of
    Nothing -> Map.lookup i dir1
    je      -> je

-- What if we had function:
altMaybe :: Maybe a -> Maybe a -> Maybe a
altMaybe ma mb =
  case ma of
    Nothing -> mb
    ja      -> ja

findEmployee' i dir0 dir1 =
  Map.lookup i dir0 `altMaybe` Map.lookup i dir1

-- What if we didn't know which file to open?
openAnyOfThese :: FilePath -> FilePath -> IO Handle
openAnyOfThese f0 f1 =
   openFile f0 ReadMode `catchIOError` \ _ -> openFile f1 ReadMode
    where catchIOError :: IO a -> (IOError -> IO a) -> IO a
          catchIOError = catchException

-- What if we had a function:
altIO :: IO a -> IO a -> IO a
altIO io0 io1 = io0 `catchIOError` \ _ -> io1
    where catchIOError :: IO a -> (IOError -> IO a) -> IO a
          catchIOError = catchException

openAnyOfThese' f0 f1 = openFile f0 ReadMode `altIO` openFile f1 ReadMode

-- Using our alternatives...
findEmployee'' i dir0 dir1 = Map.lookup i dir0 <|> Map.lookup i dir1

openAnyOfThese'' f0 f1 = openFile f0 ReadMode <|> openFile f1 ReadMode
