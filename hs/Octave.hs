{-# LANGUAGE OverloadedStrings #-}

module Main where

import Haste.Foreign
import Haste.Prim (toJSStr)
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Control.Applicative hiding (many, (<|>))
import OctaveLexer

_statement_separator :: Parser ()
_statement_separator = (char ';' <|> char '\n') >> _ws

_statements :: Parser String
_statements = do
    statements <- (_id <|> _stringLiteral <|> _int) `sepBy` (_statement_separator)
    return (foldr (++) "" statements)

-- The whole parser
theParser:: Parser String
theParser = _ws >> _statements <* eof


-- Utility function
parseIt :: String -> Either ParseError String
parseIt = parse theParser "(source)"

alive :: String -> IO String
alive _ = return "Hei, I am alive"

justLex :: String -> IO String
justLex s = return $
          case parsed of
            Right value -> "It works! just lexed: " ++ value
            Left theError -> let
              errorString = foldr (++) "" (fmap messageString (errorMessages theError))
              in "Just got: " ++ errorString
          where parsed = parseIt s


main :: IO ()
main = do
  export (toJSStr "alive") alive
  export (toJSStr "justLex") justLex
