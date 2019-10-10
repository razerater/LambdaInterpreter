# Lambda Calculus Interpreter and Reducer

Authors: Raz Reed, Alston Chau

This program will interpret any given lambda calculus expression and apply alpha-renaming, beta-reduction and eta-reduction techniques to fully reduce it. We wrote it using [Haskell](https://www.haskell.org/). To run it yourself, `git clone` this directory and simply run
```
runghc main.hs
```
To try it on your own lambda calculus expressions, either edit `input.lambda` or run it with another file:
```
runghc main.hs <input_file>
```
