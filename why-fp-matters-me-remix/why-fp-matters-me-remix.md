% YA Why FP matters
% Damian Nadales
% March 28, 2017

# About

## Objective

- What is FP and why should I care:
    - as a developer
    - as a (team) leader
        - manager
        - architect 
        - product owner

## So why?

- Productivity:
    - Less time to working code.
- Sustainable software:
    - Maintainable.
    - Extensible.
    - Performant.
- Concurrency:
    - Be future proof.

<!-- In the remaining of the presentation I'll try to present the arguments for -->
<!-- this. -->

## Disclaimer

- I'm not partial. 
    - Although I've worked with OO/Imperative for a long time.
- These *claimed* advantages hold for *me* (and other developers out there, but
  remain anecdotal).
- I cannot convince you in 30 minutes, I only want to spark your curiosity.

# What is FP

## A definition

- Structure: denotational.
    - nested expressions.
    - each expression *denotes* something.
    - the meaning depends on (evaluating)  the sub-expressions.
- Computation: evaluation of function.
- Avoids mutable data.
- Side effects (IO) pushed to the boundaries.

## Let's see an example
Determine if a list is included in another.

## A functional solution
```haskell
isSublist [] ys = True
isSublist (x:xs) [] = False
isSublist (x:xs) (y:ys) = 
    (y:ys) `beginsWith` (x:xs) || (x:xs) `isSublist` ys

beginsWith ys [] = True
beginsWith [] xs = False
beginsWith (y:ys) (x:xs) = x == y && ys `beginsWith` xs
```

## An imperative solution
```java
private static <T> boolean includes(List<T> longList, List<T> shortList) {
        if (shortList.isEmpty()) {
            return true;
        }

        outer:for (int i = 0; i < longList.size() - shortList.size() + 1; i++) {
            if (longList.get(i).equals(shortList.get(0))) {
                for (int j = 0; j < shortList.size(); j++) {
                    if (!longList.get(i + j).equals(shortList.get(j))) {
                        continue outer;
                    }
                }
                return true;
            }
        }
        return false;
    }
```

## About the functional solution

- Program the way you think vs program the way the machine thinks.
- It is declarative: I say what to compute, not how.
- We can reason about its correctness:
    - Less probability of introducing errors.
- It places us at a higher level of abstraction.
    - Huge impact in productivity (no need to worry about low level details)
    - Imagine yourself writing assembly in 2017!

# On the conciseness of FP

## Another example: toy OCR

- Write a program to recognize digits like the following:

```
" _     _  _     _  _  _  _  _ ",
"| |  | _| _||_||_ |_   ||_||_|",
"|_|  ||_  _|  | _||_|  ||_| _|",
" _     _  _     _  _  _  _  _ ",
"| |  | _| _||_||_ |_   ||_||_|",
"|_|  ||_  _|  | _||_|  ||_| _|",
"                              "
```

## Imperative/OO solution to OCR

```java
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

final class OpticalCharacterReader {
    private static final int LETTER_WIDTH = 3;
    private static final int LETTER_HEIGHT = 4;

    private static Map<Letter, String> NUMBERS = new HashMap<>();

    static {
        List<String> in = Arrays.asList(
                " _     _  _     _  _  _  _  _ ",
                "| |  | _| _||_||_ |_   ||_||_|",
                "|_|  ||_  _|  | _||_|  ||_| _|",
                "                              "
        );

        for (int i = 0; i < 10; i++) {
            NUMBERS.put(OpticalCharacterReader.getRawCharAt(in, 0, i), String.valueOf(i));
        }
    }

    private static Letter getRawCharAt(List<String> input, long line, long pos) {
        String[] result = new String[LETTER_HEIGHT];

        for (int y = 0; y < LETTER_HEIGHT; y++) {
            result[y] = input.get((int)line * LETTER_HEIGHT + y)
                    .substring((int)pos * LETTER_WIDTH, (int)(pos + 1) * LETTER_WIDTH);
        }

        return new Letter(result);
    }

    public String parse(List<String> input) {
        if (input.size() == 0 || input.size() % LETTER_HEIGHT != 0) {
            throw new IllegalArgumentException("Number of input rows must be a positive multiple of " +
                    LETTER_HEIGHT);
        }

        if (input.get(0).length() == 0 || input.get(0).length() % LETTER_WIDTH != 0) {
            throw new IllegalArgumentException("Number of input columns must be a positive multiple of " +
                    LETTER_WIDTH);
        }

        return LongStream.range(0, input.size() / LETTER_HEIGHT).boxed()
                .map(y -> LongStream.range(0, input.get(0).length() / LETTER_WIDTH).boxed()
                        .map(x -> NUMBERS.getOrDefault(getRawCharAt(input, y, x), "?"))
                        .collect(Collectors.joining()))
                .collect(Collectors.joining(","));
    }
}

class Letter {
    private final String[] input;

    Letter(String[] input) {
        this.input = input;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Letter letter = (Letter) o;

        return Arrays.equals(input, letter.input);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(input);
    }
}
```

## Functional solution to OCR

```haskell
module OCR (convert) where

import Data.List (transpose, intercalate)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map

convert :: String -> String
convert = intercalate "," . map recognizeRow . chunksOf 4 . lines
  where
    recognizeRow = map recognize . fingerprint
    recognize x = Map.findWithDefault '?' x digits
    fingerprint = transpose . map (chunksOf 3)
    digits = Map.fromList (fingerprint reference `zip` ['0'..])
    reference =
      [ " _     _  _     _  _  _  _  _ "
      , "| |  | _| _||_||_ |_   ||_||_|"
      , "|_|  ||_  _|  | _||_|  ||_| _|"
      , "                              "
      ]
```

## What did just happen?

## More reuse 

- Functional programs promote high degree of code re-use
    - use of high order functions
    - laziness: separate termination conditions from the rest of computations

## Why care about reuse?

- Faster development cycles.
    - In the previous example the functional code was 3 times smaller!
- Less probabilities of introducing errors
    - Since we don't have to re-invent the wheel.
    - Re-used functions are tested more extensively.

## Elegance

- Being declarative and using higher order abstractions allow us to write
  elegant code.

## Why do we care about elegance?

- Happier developers.
- People satisfied with their work are more productive.
- For management: reduced costs having to replace people that leave the team.

# Strong typing

## Death to null

```java
public Foo getFoo(i: Int) {...}
```

What if the function returns `null`?

## In FP is standard practice to be explicit about this

```haskell
getFoo :: Int -> Maybe Foo
```

- This means you can get either a `Foo` or Nothing.
- But no `null`!
- The client has to handle this explicitly!

## Stringly typed

```java
public Foo makeFoo(name: String, address: String) {...}
```

- What if we swap the arguments inadvertently? (based on a true story)

## Make your types strong

```haskell
newtype Name = Name String
newtype Address = Address String
makeFoo :: Name -> Address -> Foo
```
- No runtime costs for these type aliases.
- Now is no longer possible to swap arguments.

## On the hidden cost of bad/no typing

- Time to fix testing error:
    - Average: minutes
    - Worst case: hours.
    - Plus time spent writing tests.
- Time to fix compile time error:
    - Average: seconds.
    - Worst case: minutes.

## What other developers say about strong typing?

- If it compiles in most cases it just works.
- Catches all sort of nasty bugs:
    - You get tests for free
- Refactor without fear.

# Purity

## About purity

- Pure functions cannot have side effects.
- Given the same input they'll produce the same output.

## Why care about purity?

```java
r = obj.f()
```

- The result might depend on:
    - the state of `obj`.
    - the state of global variables accessed by `f`.
- We cannot be sure about post-state.
- Just think about the cognitive load.
    - Harder to reason about.
    - Harder to change (impact analysis anyone?).
    - Fallacy: Imperative/OO is simple (how?!)

## Everybody lies

```java
public A f<A>(A x);
```

- `f` could do anything:
    - mutate a global variable.
    - print on the screen.
    - eat your lunch.

## We all know what `f` is

```haskell
f :: a -> a
```
- We can be pretty sure `f` is the identity function.
- Purity + types:
    - Confidence in the code.

## What purity brings

- Isolate side effects (IO).
- Keep business logic pure.
- Less cognitive load:
    - Easier to reason about.
    - Easier to change.
    
## A practical example: big data

- Google's success: map reduce.
    - Made possible due to a functional model.
    - No data sharing enables parallelization.
- Apache spark exploits this functional model.

# Composable abstractions

## Idea

To show you how FP allows to compose abstractions in an unprecedented way.

## An HTTP1.1 parser

A parser for prefixes of HTTP 1.1 requests (e.g. "HTTP/1.1\r\n")

```c++
void Response::ProcessStatusLine( std::string const& line )
{
    const char* p = line.c_str();

    while( *p && *p == ' ' )
        ++p;

    while( *p && *p != ' ' )
        m_VersionString += *p++;
    while( *p && *p == ' ' )
        ++p;

    std::string status;
    while( *p && *p != ' ' )
        status += *p++;
    while( *p && *p == ' ' )
        ++p;

    while( *p )
        m_Reason += *p++;

    m_Status = atoi( status.c_str() );
    if( m_Status < 100 || m_Status > 999 )
        throw Wobbly( "BadStatusLine (%s)", line.c_str() );

    if( m_VersionString == "HTTP:/1.0" )
        m_Version = 10;
    else if( 0==m_VersionString.compare( 0,7,"HTTP/1." ) )
        m_Version = 11;
    else
        throw Wobbly( "UnknownProtocol (%s)", m_VersionString.c_str() );

    m_State = HEADERS;
    m_HeaderAccum.clear();
}
```

## A functional counterpart 

```haskell
httpResponseParser = (,) <$> (string "HTTP/" *> number <* string ".") 
                         <*> number
```

- We are composing parsers!

# Typing and purity

## Multi-threading is coming 

- Moore's law is now:
    - 2x cores/2 years
- 8 cores now
    - 256 cores in 10 years.
    - 8192 cores in 20 years.

[See here](http://gbaz.github.io/slides/hurt-statictyping-07-2013.pdf)

## The four horsemen of the parallel apocalypse

- Race conditions
- Deadlocks
- Livelocks
- Priority inversions

## STM to the rescue

## Moving windows

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

## This is an example of compositionality!

```haskell
swapWindows disp w a v b = atomically $ do 
  moveWindowSTM disp w a b
  moveWindowSTM disp v b a
```

Made possible because we know we cannot touch do any IO inside `STM`.

## Problem with implementing STM

- Performing non-transactional side effects in a transaction.
- Accessing transactional variables not in a transaction.

## A tale of two STM

- C#:
    - Many developers.
    - 2 years.
    - FAILURE.
- Haskell:
    - Few developers (SPJ & a student)
    - "a long weekend"
    - SUCCESS.
- Why? Static typing and purity.

## Being future proof

- Multi-threading is coming.
- Use functional programming to tame it.

# Conclusions

## FP in three bullets

- Productivity:
    - Less time to working code.
- Sustainable software:
    - Maintainable.
    - Extensible.
    - Performant.
- Concurrency:
    - Be future proof.

## Who is using FP

Almost all major players, most notably:

- Google
- Twitter
- Facebook
- WhatsApp

## I want to know more!

We want to start an FP course:

- One hour per-week to get together.
- One hour per-week to do the exercises.

## But I don't have time!

- 7 habits of highly effective people: sharpen the saw
- Consider how much money you'd be saving to the company.
    - The course is morally correct ;)
