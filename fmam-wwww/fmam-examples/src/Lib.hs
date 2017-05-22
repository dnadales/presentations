{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Control.Concurrent.Async
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as L
import           Data.Char
import           Network.HTTP.Conduit

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- * Let's start with the billion dollar mistake.

data Foo = Foo { fooName :: String }

-- | Get a Foo.
getFoo :: Int -> Maybe Foo
getFoo = undefined

-- | Capitalize a @String@.
capitalize :: String -> String
capitalize = map toUpper

-- | Capitalize a @Foo@.
capitalizeFoo :: Foo -> Foo
capitalizeFoo (Foo name) = Foo (capitalize name)

-- | Get a Foo and convert its name to upper case.
screamFooName :: Int -> Maybe Foo
screamFooName i =
  case getFoo i of
    Just foo -> Just (capitalizeFoo foo)
    Nothing  -> Nothing

-- | @screamFooName@ using @fmap@.
screamFooName' i = capitalizeFoo <$> getFoo i

-- * Let's cause side effects.
upperFile :: String -> IO String
upperFile path = do
  contents <- readFile path
  return (capitalize contents)

-- | @upperFile@ using @fmap@
upperFile' path = capitalize <$> readFile path

-- * The good old lists.
inc :: Num a => [a] -> [a]
inc []     = []
inc (n:ns) = (n+1):(inc ns)

inc' :: Num a => [a] -> [a]
-- WARNING: the type of `inc'` is more generic as it can operate over any
-- functor.
inc' = ((+1) <$>)
