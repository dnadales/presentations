% Functors, Monoids, Applicatives, and Monads: What, Why, When, Where?
% Damian Nadales
% June 1, 2017

# Functors

## This story starts with the billion dollar mistake

```java
public Foo getFoo(i: Int) {...}
```

# Applicatives

## Parsing

TODO: show a parser in the slides.

```haskell
newtype Parser s a = P { runP :: s -> Maybe (a, s) }
```

From:
/Users/damian.nadales/Documents/github/capitanbatata/functional-systems-in-haskell/fsh-exercises/src/Parsing.hs
