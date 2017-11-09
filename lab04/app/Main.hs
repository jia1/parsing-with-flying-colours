module Main where

import           Lab04
import           Text.Parsec (ParseError)

main :: IO ()
main =
  let
    parse = parseAll expr
    examples =
      [ "\\x . y"
      , "\\x y . y x"
      , "x"
      , "x y"
      , "let x = y in x"
      , "let y = \\x . x in f y y"
      ]
    expectedParsedExamples =
      map
        Right
        [ Fun "x" (Var "y")
        , Fun "x" (Fun "y" (FApp (Var "y") (Var "x")))
        , Var "x"
        , FApp (Var "x") (Var "y")
        , Let "x" (Var "y") (Var "x")
        , Let "y" (Fun "x" (Var "x")) (FApp (FApp (Var "f") (Var "y")) (Var "y"))
        ] :: [Either ParseError Term]
    parsedExamples = map parse examples
    examplesAst = map show parsedExamples
    expectedExamplesAst = map show expectedParsedExamples
    isParseCorrect = parsedExamples == expectedParsedExamples
  in
    do
      putStrLn "Results:"
      mapM_ putStrLn examplesAst
      putStrLn "Expected results:"
      mapM_ putStrLn expectedExamplesAst
      putStrLn ("Parse result: " ++ if isParseCorrect then "Correct" else "Incorrect")

