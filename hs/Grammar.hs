{-# LANGUAGE OverloadedStrings #-}

module Grammar where

import           AST
import           Data.Map.Strict
import           Expr
import           Lexer
import           Symtable
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Error


_statement :: Parser Expr
_statement = _ws >> (_expr)

_statements :: Parser [Expr]
_statements = endBy _statement _eos


errorAsMessages :: ParseError -> [String]
errorAsMessages theError = fmap messageString (errorMessages theError)
