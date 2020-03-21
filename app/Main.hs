module Main where

import System.Environment
import Text.ParserCombinators.Parsec

import Parse

readExpression :: String -> String
readExpression input =
  case parsingResult of
    Left err    -> "No match: " ++ show err
    Right value -> "Found value: " ++ show value
  where
    parsingResult = parse parseExpression "lisp" input

main :: IO ()
main = do
  (expression:_) <- getArgs
  putStrLn (readExpression expression)
