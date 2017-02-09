% Why using Haskell for doing functional-programming
% Damian Nadales
% February 6, 2017

# Prerequisites

## We share some common ground 

- You have already heard why functional-programming matters: writing software
  that is:
    - modular
    - maintainable 
    - understandable
    - testable
    - enjoyable to write
  
- You want to find out whether this is indeed true.

## We share some common [goals](https://github.com/conal/talk-2014-lambdajam-denotational-design)

- Abstractions:
    - Precise.
    - Elegant.
    - Reusable.
- Implementations:
    - Correct.
    - Efficient.
    - Maintainable
- Documentation:
    - Clear.
    - Simple.
    - Accurate.
    
## What is functional programming

- An abused term:
    - Lambdas?
    - Map, filter, reduce?
    - Category theory?
    - Programming with functions? 

- What I understand is inline with *denotative* programming:
    - Nested expression structure
    - Each expression *denotes* something
    - Depending only on denotations of sub-expressions.

## A non-denotative program
A parser for prefixes of HTTP 1.1 requests (e.g. "HTTP/1.1\r\n")

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

## A denotative counterpart

```haskell
httpResponseParser = (,) <$> (string "HTTP/" *> number <* string ".") 
                         <*> number
```

# About this presentation

## Goals

Show:

- An *incomplete* list of Haskell features *I* like.
- Why *I think* it is suitable for learning and doing serious FP.

# Before we start

## Haskell sucks

- Less libraries.
- Smaller community.
- String, Text, ByteString, argh!
- Namespace could be improved.
- Lot's of compiler extensions.
- Lazy: difficult to reason about space-complexity (althoug there are
    good profiling tools)

## Haskell is not perfect

But is the best language I know.

## Haskell misconceptions

- Haskell is hard:
    - The language has barely 22 constructs (the language doesn't even has exceptions).
    - It is functional-programming what is hard.
        - Unlike OO (dependency injection frameworks, gang-of-four patterns, concurrency)
- Haskell is not a GP language:
    - every domain in which Java is used Haskell can be used as well!
- Haskell is used only in academia. That was true in the nineties.

# Haskell is simple

## On the simplicity of Haskell

- Every feature of the language was carefully designed.
- No need to maintain compatibility with a virtual machine.
- No need to please anybody.
- Haskell supports *only one* paradigm.

## 

![](images/show-me-the-money.png)

## Function declaration

```haskell
hello = "Hello world"
```

No `function`, `def`, `{}`, `:`, etc.

## Function application is juxtaposition

In a functional language function application is one of the most used
operators.

```haskell
f x y = 2 * x + y
g z = f z z -- Equivalent to: (f z) z
```

```scala
def f(x: ???, y: ???): ??? = 2 * x + y
def g(z: ???): ??? = f (z, z)
```

## Curried by default

No need to decide between:

```scala
def willINeedCurry(x, y, z) = ???
```

```scala
def willINeedCurry(x)(y)(z) = ???
```

```haskell
thisIsCurried x y z = undefined
```

## Infix operators

Ready to use!

```haskell
like a b = a ++ " like " ++ b
like "dogs" "meat"
"pandas" `like` "bamboo"
```

## Separation function types and definitions

```haskell
sayHello :: String -> String
sayHello name = "Hello " ++ name
```

## Partial application, and point free syntax:
```haskell
sayHello = ("Hello " ++)
```

## Function composition is the dot

```haskell
screamHello = sayHello . capitalize
```

Because `andThen` is way too long...

## Lists

Lists are the workhorse of functional-programming.
```haskell
countries = [ "The Netherlands", "Poland"
            , "Turkey", "Belgium", "Italy"
            , "India", "Argentina"]
evenNumbers = [0, 2, 4]
```

```haskell
oddNumbers = [ i * 2 + 1 | i <- [0..]]
pairs = [(i,j) | i <- [1,2], j <- [1..4]]
```

Can't get any more mathematical than this.

## Lambda expressions

```haskell
duplicate = map (\x -> x ++ x)
res0 = duplicate ["foo", "bar"]
```

## Haskell is declarative

```haskell
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [b | b <- xs, b <= x] ++ [x] ++ qsort [a | a <- xs, x < a]
```

. . . 

```scala
def qsort[T <% Ordered[T]](list: List[T]): List[T] = {
  list match {
  case Nil => Nil     
  case x::xs =>        
    val (before, after) = xs partition (_ < x)
    qsort(before) ++ (x :: qsort(after))
  }
}
```

## For comprehensions in Scala
```haskell
def inc: IRWS[Int, List[String], Counter, Counter, Unit] = for {
  v <- SM.ask
  c <- SM.get
  _ <- SM.tell(List(s"Incrementing $c by $v "))
  _ <- SM.modify(counter ⇒ Counter(counter.value + v))
} yield ()
```

## Do notation in Haskell

```haskell
inc :: (MonadReader Int m, MonadState Counter m, MonadWriter [Counter] m) => m ()
-- inc = ask >>= incWith >> get >>= tell . (:[])
inc = do
  v <- ask
  incWith v
  s <- get
  tell [s]
```

- No `yield` required the end.
- No dummy `<-`.

# Direct support for FP

## Purity is crucial to FP

- Haskell has no assignment and no  global variables.
- No IO in pure code:
```haskell
myPureFunction :: String -> String
myPureFunction xs =
if xs == [] then putStrLn "Empty String!"; [] else xs
```
- Enforces discipline.
- Forces you to think in a different way.

## No silver bullet

Of course, you can still write awful code in Haskell.


## Algebraic Data Types

```haskell
data List a = Nil | Cons a (List a)
```

. . .

Compare with:

```scala
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
```

## See? Less concepts!

No need to worry about:

- Variance.
- Classes, objects, inheritance.
- Type parameters.
- Lots of symbols!

## Mini-challenge

Can you write a definition for `prepend` in Scala without googling it up? Hint:

```scala
def prepend(elem: ???): ??? = new Cons(elem, this)
```

## Typeclasses
Full blown FP makes extensive use of *typeclasses*.

## A Monoid written in Scala:
```scala
trait Monoid[A] {
  def empty: A
  def combine(x: A, y: A): A
}
```

And its instance:
```scala
// Implementation for Int
val intAdditionMonoid: Monoid[Int] = new Monoid[Int] {
  def empty: Int = 0
  def combine(x: Int, y: Int): Int = x + y
}
```

## A Monoid written in Haskell

```haskell
class Monoid a where
  mempty  :: a
  mappend :: a -> a -> a
```

And its instance
```haskell
instance Monoid Int where
  mempty = 0
  mappend = +
```

Call things for what they are: is not a plane, it is not a bird, it is not a
`def`, it is not a `val`, it is an `instance`!

## Typeclasses can be used to constrain functions

```haskell
class Ord a where
  (<=) :: a -> a -> Bool

sort :: Ord a => [a] -> [a]
sort = undefined
```

## Derived instances

When things start to get hairy

![](images/hairy.png)

## Deriving instances in Scala

```scala
final case class Pair[A, B](first: A, second: B)

def deriveMonoidPair[A, B](A: Monoid[A], B: Monoid[B]): Monoid[Pair[A, B]] =
  new Monoid[Pair[A, B]] {
    def empty: Pair[A, B] = Pair(A.empty, B.empty)

    def combine(x: Pair[A, B], y: Pair[A, B]): Pair[A, B] =
      Pair(A.combine(x.first, y.first), B.combine(x.second, y.second))
  }
```

## 

![](images/jackie.jpg)

## Deriving instances in Haskell

```haskell
instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty) -- No A.empty, B.empty, let the compiler work for
                            -- us and we use our time to do cool stuff.
  mappend (a1, b1) `mappend` (a2, b2) =
    (a1 `mappend` a2, b1 `mappend` b2)
```

## Partial application on types: Scala

```scala
def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
  def unit[A](a: => A): State[S,A] = ???
  def flatMap[A, B](st: State[S, A])(f: A => State[S,B]): State[S, B] = ???
}
```

Yes, I know there is a kind-projector plugin, but still it is more awkward
than...

## Partial application on types: Haskell

```haskell
instance Monad (State s) where
  return x = undefined
  sa >>= fsb = undefined
```

## Composing effects via Monad transformers

```haskell
newtype Counter = Counter Int deriving (Eq)

instance Show Counter where
  show (Counter i) = show i

incWith :: MonadState Counter m => Int -> m ()
incWith n = let incCounter n' (Counter i) = Counter $ i + n'
            in modify (incCounter n)

inc :: (MonadReader Int m, MonadState Counter m, MonadWriter [Counter] m) => m ()
inc = ask >>= incWith >> get >>= tell . (:[])

compute :: (MonadReader Int m, MonadState Counter m, MonadWriter [Counter] m) => m ()
compute =
  local (const 3) $ do
    inc
    inc
    inc
    local (const 5) $ do
      inc
      inc
```

## Mini-challenge 2

Implement this in Scala.

## An ad-hoc solution

```scala
import scalaz._
  import Scalaz._
  val SM = ReaderWriterState.rwstMonad[Id, Int, List[String], Counter]

  case class Counter(value: Int)

  def incWith(n: Int): State[Counter, Unit] = for {
    v <- get[Counter]
    _ <- put(Counter(v.value + n))
  } yield ()

  def inc: IRWS[Int, List[String], Counter, Counter, Unit] = for {
    v <- SM.ask
    c <- SM.get
    _ <- SM.tell(List(s"Incrementing $c by $v "))
    _ <- SM.modify(counter ⇒ Counter(counter.value + v))
  } yield ()

  def compute: IRWS[Int, List[String], Counter, Counter, Unit] = {
    for {
      _ <- SM.local[Unit](i ⇒ 3)(for {
        _ <- inc
        _ <- inc
        _ <- inc
      } yield ())
      _ <- SM.local[Unit](i ⇒ 5)(for {
        _ <- inc
        _ <- inc
        _ <- inc
      } yield ())
    } yield ()
  }
```

## Higher kinded types
With type-kinds we can abstract over types:

```haskell
data RoseTree a = RLeaf a
                | RNode (List (RoseTree a))

data BinTree a = BLeaf a
               | BNode (Pair (BinTree a))

data Pair a = MkPair a a
```

Note the similar pattern in the definitions of `RoseTree` and `BinTree`. We are
repeating ourselves! We can abstract this as follows.

##

```haskell
data Tree f a = Leaf a
              | Tree (f (Tree f a))

type RoseTree a = Tree List a
type BinTree a = Tree Pair a
```

If your language gets in your way and prevents this kind of reuse you will be
almost forced to repeat yourself because it is easier!

But the fun does not stop here. See `PolyKinds`!

# Great libraries

## Parallel and concurrent programming
    
Libraries for concurrent programming are amazing.

- Repa.
- STM.
- Haxl (Facebook).
- Pipes.
- Conduits.

## Example: STM

This is how easy it is to solve the philosophers-problem:
```haskell
moveWindowSTM :: Display -> Window -> Desktop -> Desktop -> STM ()
moveWindowSTM disp win a b = 
  wa <- readTVar ma
  writeTVar ma (Set.delete win wa)
  wb <- readTVar mb
  writeTVar mb (Set.insert win wb)
  where
    ma = disp ! a
    mb = disp ! b
```

This is an example of compositionality!

```haskell
swapWindows disp w a v b = atomically $ do 
  moveWindowSTM disp w a b
  moveWindowSTM disp v b a
```

# Great extensions

## GADT's

```haskell
data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Eq :: (Eq a) => Expr a -> Expr a -> Expr Bool
```

You can detect errors at compile time:
```haskell
eRight = (I 5 `Add` I 1) `Eq` I 7
eWrong = (B True) `Add` I 5 -- Won't type check since 'B True' does not
                            -- have type 'Expr Int'!
eAlsoWrong = I 5 `Eq` B False
```

## Mini-challenge 3
Try to define a type-safe EDSL in the language you like.

## Rank-n-types

```haskell
sumFs f xs ys = (f xs) + (f ys)
```

What is its type? The compiler will infer the following type:
```haskell
sumFs :: Num a => (t -> a) -> t -> t -> a
```

But this means we cannot simply call:
```haskell
sumFs length "hello" [0, 1, 2]
```

## Enter rank-n-types

```haskell
sumPolyFs :: (forall a. [a] -> Int) -> [b] -> [c] -> Int
sumPolyFs f xs ys = f xs + f ys
```

Now we can call:
```haskell
sumPolyFs length "hello" [0, 1, 2]
```

## Are rank-n-types useful?

Have you heard of:

- The `ST` monad.
- The free-monad.
- Lenses.

## Automatic derivations

Derive (trival) functors, monads, and the like for free!

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype App a = App { unApp :: ReaderT Config (StateT AppState IO) a }
                deriving (Monad, MonadReader Config,
                          MonadState AppState, MonadIO)
```

[See this blog post.](https://ocharles.org.uk/blog/guest-posts/2014-12-15-deriving.html)

# Great Tools 

## Stack

- Package manager.
- Build-tool.
- Scaffolding tool.

## The REPL

- `:t` type information.
- `:k` kind information (type of a type).
- `:i` information of a symbol.
- `:l` load a file.


## Hoogle

Search functions by type signature.

# Summary

## Haskell is the right tool for learning FP
 
- Less cluttered language/Less concepts
- FP concepts are directly supported (no need to write FP-assembly)
- Monad transformers are easy to compose
- Discipline: you're (kind of) forced to write functional code! No way to
  cheat with imperative code.
- Performance does not suffer for writing functional code.

## My list is incomplete

I showed an incomplete list of why I like about the language, but you might
like other aspects!

## More on Scala compared to Haskell

- [Rúnar Bjarnason: functional programming is terrible](https://www.youtube.com/watch?v=hzf3hTUKk8U)
- [The Red Book On Scala has a Wiki about Haskell](https://github.com/fpinscala/fpinscala/wiki/A-brief-introduction-to-Haskell,-and-why-it-matters)
- [On the price paid for writing Scala](https://www.reddit.com/r/haskell/comments/1pjjy5/odersky_the_trouble_with_types_strange_loop_2013/cd3bgcu/)

## Where to go from here

- I would like to organize FP-sessions.
- Could be two lunches a week.
- Learning FP in Haskell.
