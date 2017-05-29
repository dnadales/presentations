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
    - These are very general abstractions
- I'm doing my best, but you should learn this from the best.

# Functors

## This story begins with the billion dollar mistake

```java
public Address getAddress(Int i, AddressBook aBook) {...}
```

## Solving the billion dollar mistake

```haskell
newtype Address = Address { address :: String }

type AddressBook = Map Int Address

getAddress :: Int -> AddressBook -> Maybe Address
getAddress = Map.lookup

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
    Nothing             -> Nothing
    Just (Address name) -> Just (length name)
```

## We have some code duplication

```haskell
mapAddressBook :: (String -> b) -> Int -> AddressBook -> Maybe b
mapAddressBook f i aBook =
  case getAddress i aBook of
    Nothing             -> Nothing
    Just (Address name) -> Just (f name)

getAddressDigits' :: Int -> AddressBook -> Maybe String
getAddressDigits' i aBook = mapAddressBook (filter isDigit) i aBook

lengthAddress' :: Int -> AddressBook -> Maybe Int
lengthAddress' = mapAddressBook length
```

## Are we happy yet?

`mapAddressBook` is related to `Maybe` (more than addresses...)

```haskell
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing  = Nothing
mapMaybe f (Just x) = Just (f x)

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

## Let's add names

```haskell
newtype Name = Name { name :: String }

data Employee = Employee Name Address

type NameDir = Map Int Name

getName = Map.lookup
```

## The making of an employee

```haskell
getEmployee :: Int -> NameDir -> AddressBook -> Maybe Employee
getEmployee i nDir aBook =
  case getName i nDir of
    Nothing -> Nothing
    Just name ->
      case getAddress i aBook of
        Nothing      -> Nothing
        Just address -> Just (Employee name address)
```

## Reading an employee

```haskell
readEmployee :: IO Employee
readEmployee = do
  name <- getLine
  address <- getLine
  return (Employee (Name name) (Address address))
```

## Looking for patterns

- We're extracting values inside `Maybe` (if any) and `IO`.
- Then we're applying the `Employee` constructor

```haskell
Employee :: Name -> Address -> Employee
getName i nDir :: Maybe Name
getAddress i aBook :: Maybe Address

```

## Maybe we need a `fmap2`

```haskell
fmap :: (a -> b) -> Maybe a -> Maybe b
fmap2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
getEmployee' i nDir aBook = 
  Employee `fmap2` (getName i nDir) (getAddress i aBook)
```

. . .

- But what if we add other fields to employee?
- We could define `fmap3`, `fmap4`, `fmap5`, etc.
- But we like type signatures that fit in 80 characters.

## What if we would apply `fmap` partially?

```haskell
almostGetEmployee :: Int -> NameDir -> Maybe (Address -> Employee)
almostGetEmployee i nDir  = 
  Employee `fmap` (getName i nDir)
```

. . .

Wouldn't be great if...

```haskell
applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
```

## Using `applyMaybe`

```haskell
getEmployee' i nDir aBook =
  (Employee <$> getName i nDir) `applyMaybe` getAddress i aBook
```

. . .

All great but ...

. . .

Maybe `applyMaybe` is too specific!

```haskell
applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
apply      :: f     (a -> b) -> f     a -> f     b
```

## Enter Applicatives

```haskell
class Functor f => Applicative f where
  pure :: a -> f a -- Lift a value
  (<*>):: f (a -> b) -> f a -> f b -- Sequential application
```

```haskell
getEmployee' i nDir aBook =
  pure Employee <*> getName i nDir <*> getAddress i aBook

readEmployee' =
  pure Employee <*> (Name <$> getLine) <*> (Address <$> getLine)
```

# Fluffy teddy bears (a.k.a Monads)

## Adding a BSN to the mix

Imagine we'd have to lookup the employee's id by her social security number:

```haskell
type SSNDir = Map Int Int

getId = Map.lookup

getEmployeeBySSD :: Int -> SSNDir -> NameDir -> AddressBook -> Maybe Employee
getEmployeeBySSD ssn sDir nDir aBook = undefined
```

## We'd like to use our new friends ...

```haskell
getEmployeeBySSD ssn sDir nDir aBook = 
  pure Employee <*> getName i nDir <*> getAddress i aBook
  where i = getId ssn
```

. . .

But `i` has to come from the lookup operation `getId`, which can return
nothing!

## A case clause again

```haskell
getEmployeeBySSD ssn sDir nDir aBook =
  case getId ssn sDir of
    Nothing -> Nothing
    Just i  -> pure Employee <*> getName i nDir <*> getAddress i aBook
```

## If only we had a function...

```haskell
flatMapMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
flatMapMaybe ma fmb =
  case ma of
    Nothing -> Nothing
    Just x  -> fmb x
```

. . .

Then we could re-write:

```haskell
getEmployeeBySSD' ssn sDir nDir aBook =
  getId ssn sDir `flatMapMaybe`
  \i -> pure Employee <*> getName i nDir <*> getAddress i aBook
```

## `flatMap` and lists

Function `flatMap` is also needed when working with lists.

Let's write a function that returns all the permutations of a list:

```haskell
interleave :: a -> [a] -> [[a]]
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = map (interleave x) (perms xs) -- ! Won't compile
```

## Defining a `flatMap` for lists

```haskell
flatMapList :: [a] -> (a -> [b]) -> [b]
flatMapList xs f = concat (map f xs)

perms []     = [[]]
perms (x:xs) =
  flatMapList (perms xs) (interleave x)
```

## It seems we can flat-map a lot of things

```haskell
flatMapMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
flatMapList  :: []    a -> (a -> []    b) -> []    b
flatMapIO    :: IO    a -> (a -> IO    b) -> IO    b
```

We can abstract over the type constructors as well!

. . . 

```haskell
flatMap :: f a -> (a -> f b) -> f b
```

## Enter Monads

```haskell
class Applicative m => Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
```

## Re-writing our monadic programs

```haskell
getEmployeeBySSD'' ssn sDir nDir aBook =
  getId ssn sDir >>=
  \i -> pure Employee <*> getName i nDir <*> getAddress i aBook

perms' []     = [[]]
perms' (x:xs) = perms xs >>= interleave x
```

## Do it again

Monads are so pervasive in Haskell that there is syntactic sugar for using
them:

```haskell
do x <- m
   e
   
-- Desugars to:

m >>= \x -> e
```

## Why do?

Do-notation is useful when having to compose bind operators (`>>=`):

```haskell
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
```

## Re-do-ing our previous programs

```haskell
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
```

# Alternative

## Find my employees!

What if we didn't know where data come from?

```haskell
findEmployee :: Int -> Map Int Employee -> Map Int Employee -> Maybe Employee
findEmployee i dir0 dir1 =
  case Map.lookup i dir0 of
    Nothing -> Map.lookup i dir1
    je      -> je
```

## Duplication again!
We have a trained eye for spotting code duplication by now...

```haskell
altMaybe :: Maybe a -> Maybe a -> Maybe a
altMaybe ma mb =
  case ma of
    Nothing -> mb
    ja      -> ja
    
findEmployee' i dir0 dir1 =
  Map.lookup i dir0 `altMaybe` Map.lookup i dir1
```

## Find a file to open!

What if we didn't know which file to open?

```haskell
openAnyOfThese :: FilePath -> FilePath -> IO Handle
openAnyOfThese f0 f1 =
   openFile f0 ReadMode `catchIOError` \ _ -> openFile f1 ReadMode
    where catchIOError :: IO a -> (IOError -> IO a) -> IO a
          catchIOError = catchException
```

## This code belongs to IO!

```haskell
altIO :: IO a -> IO a -> IO a
altIO io0 io1 = io0 `catchIOError` \ _ -> io1
    where catchIOError :: IO a -> (IOError -> IO a) -> IO a
          catchIOError = catchException

openAnyOfThese' f0 f1 = openFile f0 ReadMode `altIO` openFile f1 ReadMode
```

## Looking for alternatives

```haskell
altMaybe :: Maybe a -> Maybe a -> Maybe a
altIO    :: IO    a -> IO    a -> IO    a
```

## Enter Alternatives

```haskell
class Applicative f => Alternative f where
  empty :: f a               -- Identity of <|>
  (<|>) :: f a -> f a -> f a
```

## Using our alternatives

```haskell
findEmployee'' i dir0 dir1 = Map.lookup i dir0 <|> Map.lookup i dir1

openAnyOfThese'' f0 f1 = openFile f0 ReadMode <|> openFile f1 ReadMode
```


# Parsing

## Parsing HTTP responses

```java
public class HttpResponseParser extends AbstractMessageParser {

    private final HttpResponseFactory responseFactory;
    private final CharArrayBuffer lineBuf;

    public HttpResponseParser(
            final SessionInputBuffer buffer,
            final LineParser parser,
            final HttpResponseFactory responseFactory,
            final HttpParams params) {
        super(buffer, parser, params);
        if (responseFactory == null) {
            throw new IllegalArgumentException("Response factory may not be null");
        }
        this.responseFactory = responseFactory;
        this.lineBuf = new CharArrayBuffer(128);
    }
    
    protected HttpMessage parseHead(
            final SessionInputBuffer sessionBuffer)
        throws IOException, HttpException, ParseException {

        this.lineBuf.clear();
        int i = sessionBuffer.readLine(this.lineBuf);
        if (i == -1) {
            throw new NoHttpResponseException("The target server failed to respond");
        }
        //create the status line from the status string
        ParserCursor cursor = new ParserCursor(0, this.lineBuf.length());
        StatusLine statusline = lineParser.parseStatusLine(this.lineBuf, cursor);
        return this.responseFactory.newHttpResponse(statusline, null);
    }
}
```

## FP style

```haskell
httpResponseParser = (,) <$> (string "HTTP/" *> number <* string ".") 
                         <*> number
```


## A simple parser 
TODO: Maybe $ show a parser in the slides.

```haskell
newtype Parser s a = P { runP :: s -> Maybe (a, s) }
```

# Epilogue 

## Laws

- Functors, applicatives, monads, and alternatives instances must obey laws:
    - Help reasoning about programs and building intuition.
    - Guide the implementation of these instances.
- Due to time constraints I didn't discuss them here.

## Conclusions

- Functors, applicatives, monads, and alternatives allows high degree of code
  reuse.
      - Smaller code-bases.
      - Less probability of introducing errors.
- It might take time to learn them but it pays off.
- There is more: foldables, traversables, co-monads, categories, etc.


## Further reading

-
  [The monad tutorial fallacy](https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/)
- [Functors, Applicatives, And Monads In Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)
- [Dr Frankenfunctor and the Monadster (F#)](https://fsharpforfunandprofit.com/posts/monadster/)
- [The typeclassopedia](https://wiki.haskell.org/Typeclassopedia)
