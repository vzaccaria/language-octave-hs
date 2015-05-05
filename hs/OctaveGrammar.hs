{-# LANGUAGE OverloadedStrings #-}

module OctaveGrammar where

import Text.Parsec.Expr
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Control.Applicative hiding (many, (<|>))
import Debug.Trace
import PrettyPrint

import OctaveAST
import OctaveLexer




_statement :: Parser String
_statement = _ws >> (_id <|> _stringLiteral)

_statements :: Parser [String]
_statements = endBy _statement _eos


errorAsMessages :: ParseError -> [String]
errorAsMessages theError = fmap messageString (errorMessages theError)

theParser:: Parser String
theParser = _ws >> (fmap asSet _statements) <* eof



-- Utility function (to string)
parseIt :: String -> Either ParseError String
parseIt = parse theParser "(source)"

justParse :: String -> IO String
justParse s = return $
          case parsed of
            Right value -> "It works! just lexed: " ++ value
            Left theError -> let
              errorString = asSet $ errorAsMessages theError
              in "Error!! " ++ errorString
          where parsed = parseIt s
