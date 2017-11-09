# Lab 04 Parser for Lambda Expressions in Haskell

This will be done on Lab/Tutorial session on 6 Nov 2017
Submission by: 5pm 17 Nov 2017

## Parser for Lambda Calculus : 80%

Using the parser combinators provided in the Parsec library, design a parser for
the lambda calculus with the let-construct.

The BNF of the language is as follows:

```
<lam-expr> ::= <identifier>
             | <lam-expr> <lam-expr>
             | (<lam-expr>)
             | let <identifier> = <lam-expr> in <lam-expr>
             | \ <identifier>+ . <lam-expr>
```

For applications, you must make sure that it associates to the left. (Hint: you
may make use of the REPL to try to get a number of lambda terms before forming
nested binary applications.) For lambda abstractions, you must make sure that it
extends as far as possible to the right. We have provided the following
algebraic data type to model the abstract syntax for your lambda terms:

```haskell
data Term
  = Var String
  | Fun String Term
  | FApp Term Term
  | Let String Term Term
  deriving (Show, Eq)
```

Your task is to complete the parser, and support multi-argument lambda
abstractions of the form `(\x y z . x (y z))` which will be parsed as:

```haskell
Fun "x" (Fun "y" (Fun "z" (FApp (Var "x") (FApp (Var "y") (Var "z")))))
```

You are provided with some stub code in `Lab04.hs`; it contains some basic
parsers that you may make use of in your solution. We have also provided an
example parser for a lambda abstraction.



We have given you some simple test cases in `Main.hs`. Please design additional
test cases for your parser.

## Lambda Evaluators : 20%

Write an interpreter that works on the given abstract syntax. The interpreter
should use either call-by-value or call-by-name semantics to reduce a given term
into a normal form. It should not attempt to evaluate expressions under a lambda
abstraction. You will need to support beta-reduction and alpha-renaming
operations for your interpreter to work.

Complete the following 2 functions for this section:

```haskell
evalByName :: Term -> Term
evalByValue :: Term -> Term
```

You may use the state monad at Control.Monad.State to help
generate a fresh supply of names.

## Bonus : 20%

Using a new program, called Lab04Bonus.hs, write a version of
the program that would display the execution tracing for
lambda evaluation. You are free to use any format for
such execution tracing, including graphical means.

## Submission

Please submit a zip file, which must contain:
  - The completed `Lab04.hs`
  - `Lab04Bonus.hs`, if done.
  - A short 1-page report describing your experience implementing
    this parser and evaluator via Haskell.
