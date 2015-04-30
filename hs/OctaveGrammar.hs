{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Control.Applicative hiding (many, (<|>))
import OctaveLexer
import PrettyPrint

_statement :: Parser String
_statement = _id <|> _int

_statements :: Parser [String]
_statements = endBy _statement _eos

-- The whole parser parses statements

errorAsMessages :: ParseError -> [String]
errorAsMessages theError = fmap messageString (errorMessages theError)

theParser:: Parser String
theParser = _ws >> (fmap asSet _statements) <* eof

-- Utility function
parseIt :: String -> Either ParseError String
parseIt = parse theParser "(source)"

justLex :: String -> IO String
justLex s = return $
          case parsed of
            Right value -> "It works! just lexed: " ++ value
            Left theError -> let
              errorString = asSet $ errorAsMessages theError
              in "Error!! " ++ errorString
          where parsed = parseIt s


main :: IO ()
main = do
  s <- justLex "vittorio\nabcde"
  putStr s
