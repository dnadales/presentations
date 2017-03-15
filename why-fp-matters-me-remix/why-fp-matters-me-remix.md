% Why FP matters (me remix)
% Damian Nadales
% March 28, 2017

# Introduction

## Objective

- What is FP and why should I care:
  - as a developer
  - as a (team) leader
    - manager
    - architect 
    - product owner

## So why?

- If you're a developer: enjoy (y)our work.
- If you're a manager: make more money.

In the remaining of the presentation I'll try to present the arguments for
this.

## Disclaimer

- I'm not partial. 
- These *claimed* advantages hold for *me* (and other developers out there, but
  remain anecdotal).
- I cannot convince you in 30 minutes, I only want to spark your curiosity.

## Disclaimer

I'll try to not use cheap tricks to influence you...

## Disclaimer

... but management does not want you to hear this.

# What is FP

## A definition

- Computation: evaluation of function.
- Structure: denotational.
    - nested expressions.
    - each expression *denotes* something.
    - the meaning depends on the sub-expressions.
- Avoids mutable data.
- Avoids changing state.

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

- It is declarative: I say what to compute, not how.
- I can reason about its correctness:
    - Less probability of introducing errors.
- It places us at a higher level of abstraction.

# On the conciseness of FP

## Another example: toy OCR

Write a program to recognize digits like the following:

```
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

## What purity brings

- Isolate side effects (IO).
- Keep business logic pure.
- Less cognitive load:
    - Easier to reason about.
    - Easier to change.
  
# Composable abstractions

## Idea

To show you how FP allows to compose abstractions in an unprecedented way.

## An HTTP1.1 parser

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

## A functional counterpart 

```haskell
httpResponseParser = (,) <$> (string "HTTP/" *> number <* string ".") 
                         <*> number
```
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


# Conclusions

## FP in three bullets
- More productive developers.
- Reduce development costs.
- Reduce maintenance costs.

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
