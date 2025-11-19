# why
This is a simple lisp interpreter I don't really have any plans to make this a full feature-full lisp dialect or implementation, it was just a good intro for writing a interpreter.

# supported stuff so far
- if and else expression: (if (= 1 2 "is equal" "not equal")
- function defintions: (def sqr (lam (n) (* n n)))
- basic binary operations and bit operations (+ 1 2) (<< 1 4) (1 < 3) ... etc
- variable definitions (def meow 5)
- a few list functions: (head (1 2 3)) (tail (1 2 3)) (nil? (1 2 3)) ... etc

Everything hasn't been tested yet to ensure it properly works so except lots of bugs!

# how to run 
```
git clone <this repo>
cd lamlang
cabal run
```
Then have some fun playing with it!
