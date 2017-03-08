% Getting started with Haskell
% Damian Nadales
% March 30, 2017

# Introduction

## Objectives

- Introduce Haskell's main features.
- Get you started with Haskell tooling.
- Write some Haskell!

## Prerequisites

TODO: to be asked before the meetup.

- Stack is installed in your system.
- You created and setup a `hello-haskell` project.

# Our first Haskell project

## Stack

- Package manager.
- Build tool.
- Scaffolding tool.
- "One-stop shop for all the Haskell tooling you need".
- Aims at achieving reproducible builds.

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

```sh
$ tree hello-haskell
hello-haskell
├── LICENSE
├── Setup.hs
├── app
│   └── Main.hs
├── hello-haskell.cabal
├── src
│   └── Lib.hs
├── stack.yaml
└── test
    └── Spec.hs

3 directories, 7 files
```

## Adding additional dependencies

TODO: show how to add containers (required by Data.Map)

# A first taste of Haskell

## Let's fire up the REPL

```sh
cd hello-haskell
stack ghci
```

## Simple expressions

- Arithmetic operations.
- Strings.
- Lists.

## Declaring a function

- The `sayHello` function!
- In the REPL.
- In the `Lib.hs` file.
- We can use the REPL for quick feedback and testing.
- Reload with `:r`.

## No types?

- Let's see the type of `sayHello`.
- Use `:t` to see the types.
- We can declare types explicitly.
  - Serve more as a documentation tool.
  - Although in occasions we need to help GHC.
  
## Associativity

- Function declaration associates right.
```haskell
a -> b -> c = a -> (b -> c)
```

- Or if you prefer a mnemonic.
Avoid -> Success -> AtAllCosts = Avoid -> (Success -> AtAllCosts)

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

# First exercise: Scrabble score

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

# More material

## Pattern matching

```haskell
f (x:y:z:xs) = [x, z]
f _  = []
```

`_` is a /wildcard/ pattern. Matches everything.

## Matching by cases

```haskell
f  (Just x)
 | x == 0 = "zero"
 | x == 1 = "One"
 | otherwise = "More"
f Nothing = "Nothing!" 
```

## Flattening lists 

```haskell
concat :: [[a]] -> [a]
```

TODO:
```haskell
show and example of >>= 
```



## Intersperse

```haskell
intersperse :: a -> [a] -> [a]
```

# Second exercise: Ocr Numbers

## A short solution

Source: http://exercism.io/submissions/3c3799bc94d3446baf490acff982647b

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

## Java solution number 0

Source: http://exercism.io/submissions/176d27f6113d46d3a96194bdad92b86c

```java
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

final class OpticalCharacterReader {
    private static Map<String, Character> characterMap = new HashMap<String, Character>() {{
        put(" _ | ||_|   ", '0');
        put("     |  |   ", '1');
        put(" _  _||_    ", '2');
        put(" _  _| _|   ", '3');
        put("   |_|  |   ", '4');
        put(" _ |_  _|   ", '5');
        put(" _ |_ |_|   ", '6');
        put(" _   |  |   ", '7');
        put(" _ |_||_|   ", '8');
        put(" _ |_| _|   ", '9');
    }};

    public static String parse(List<String> strings) {
        String string = String.join(",", parseLines(strings));
        return string;
    }

    private static List<String> parseLines(List<String> strings) {
        if(strings.size() % 4 != 0) {
            throw new IllegalArgumentException("Number of input rows must be a positive multiple of 4");
        }
        ArrayList<String> lines = new ArrayList<>();
        for(int i = 0; i < strings.size(); i += 4) {
            lines.add(parseLine(strings.subList(i, i + 4)));
        }
        return lines;
    }

    private static String parseLine(List<String> line) {
        int length = line.get(0).length();
        if(length % 3 != 0) {
            throw new IllegalArgumentException("Number of input columns must be a positive multiple of 3");
        }
        String string = "";
        for(int j = 0; j < length; j += 3) {
            string += parseCharacter(line, j);
        }
        return string;
    }

    private static Character parseCharacter(List<String> line, int j) {
        String characterString = getCharacterString(line, j);
        return parseCharacterString(characterString);
    }

    private static Character parseCharacterString(String characterString) {
        if(characterMap.keySet().contains(characterString)) {
            return characterMap.get(characterString);
        }
        return '?';
    }

    private static String getCharacterString(List<String> line, int j) {
        String string = "";
        for(int i = 0; i < 4; i++) {
            string += line.get(i).substring(j, j + 3);
        }
        return string;
    }
}
```

## Java solution number 1

Source: http://exercism.io/submissions/08b891cf57544d4087b4587c403305f8

```java
import java.awt.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

final class OpticalCharacterReader {
    private final int CharacterWidth = 3;
    private final int CharacterHeight = 4;

    private final Map<String, String> CharactersMap = getCharactersMap();
    private List<String> lines;

    public String parse(List<String> lines) {
        this.lines = lines;

        if (lines.size() % CharacterHeight != 0) {
            throw new IllegalArgumentException("Number of input rows must be a positive multiple of 4");
        }

        if (lines.get(0).length() % CharacterWidth != 0) {
            throw new IllegalArgumentException("Number of input columns must be a positive multiple of 3");
        }

        int x = this.horizontalCharacters();
        int y = this.verticalCharacters();

        return IntStream.range(0, verticalCharacters())
            .mapToObj(this::rowCharacters)
            .collect(Collectors.joining(","));
    }

    private String rowCharacters(int row) {
        return IntStream.range(0, horizontalCharacters())
                .mapToObj(x -> new Point(x, row))
                .map(this::convertCharacter)
                .collect(Collectors.joining(""));
    }

    private int horizontalCharacters() {
        return lines.get(0).length() / CharacterWidth;
    }

    private int verticalCharacters() {
        return lines.size() / CharacterHeight;
    }

    private String convertCharacter(Point point) {
      return matchCharacter(character(point));
    }

    private String character(Point point) {
        return IntStream.range(0, CharacterHeight)
                .mapToObj(offset -> characterOnLine(point, offset))
                .collect(Collectors.joining(""));
    }

    private String characterOnLine(Point point, Integer offset) {
        String completeLine = lines.get(point.y * CharacterHeight + offset);
        return completeLine.substring(point.x * CharacterWidth, point.x * CharacterWidth + CharacterWidth);
    }

    private String matchCharacter(String character) {
        return CharactersMap.getOrDefault(character, "?");
    }

    private static Map<String, String> getCharactersMap() {

        Map<String, String> map = new HashMap<>();
        map.put(" _ " +
                "| |" +
                "|_|" +
                "   ",
                "0");
        map.put("   " +
                "  |" +
                "  |" +
                "   ",
                "1");
        map.put(" _ " +
                " _|" +
                "|_ " +
                "   ",
                "2");
        map.put(" _ " +
                " _|" +
                " _|" +
                "   ",
                "3");
        map.put("   " +
                "|_|" +
                "  |" +
                "   ",
                "4");
        map.put(" _ " +
                "|_ " +
                " _|" +
                "   ",
                "5");
        map.put(" _ " +
                "|_ " +
                "|_|" +
                "   ",
                "6");
        map.put(" _ " +
                "  |" +
                "  |" +
                "   ",
                "7");
        map.put(" _ " +
                "|_|" +
                "|_|" +
                "   ",
                "8");
        map.put(" _ " +
                "|_|" +
                " _|" +
                "   ",
                "9");
        return map;
    }
}
```

## Java solution number 2

Source: 

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
