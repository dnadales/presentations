-- | Companion module for the presentation.

module WhyHaskell where

import           Data.Char
import           Prelude   hiding (even)

sayHello = ("Hello " ++)

-- * Function composition

capitalize = map toUpper

screamHello = sayHello . capitalize

-- * Lists
fiveEven = [0, 2, 4, 6, 8]
allNats = [0..]
even n = n `mod` 2 == 0
fiveEven' = take 5 $ filter even [0..]
  -- Same as:
  --
  -- > take 5 (filter even [0..])

-- * Lambda expressions
duplicate = map (\x -> x ++ x)
res0 = duplicate ["foo", "bar"]

-- * ADT's
data Color = Red | Green | Blue
data MList a = Nil | Cons a (MList a)

-- Constructors are functions
cons = Cons

-- * Zero-overhead type wrappers
newtype Age = Age Int
newtype Age = Height Int
newtype Name = Name String

-- * Record syntax
data Person = Person {name :: Name, age :: Age, heightCm :: Height}
