% Why using Haskell for learning functional-programming
% Damian Nadales
% February 6, 2017

# Direct support for FP

## Build the Docker image
Let's see:

```
 docker build -t dnadales/revealjs-server .
```

## Start the docker container
``` bash
docker run -p 49160:8000  -v `pwd`:/revealjs/presentation/ --rm dnadales/revealjs-server:latest
```

Then go to:
```
http://localhost:49160/presentation/
```

## Generate the documents

```
pandoc -t revealjs -s --variable="revealjs-url":"" why-haskell-for-fp.md -o index.html
```

# Typeclasses 

``` haskell
class Hello where
```

## Tomorrow
Will be another day...


