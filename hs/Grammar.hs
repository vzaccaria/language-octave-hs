{-# LANGUAGE OverloadedStrings #-}

module Grammar where

import           AST
import           Control.Applicative                 hiding (many, optional,
                                                      (<|>))
import           Data.Map.Strict
import           Expr
import           Lexer
import           Symtable
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Error


_assign_statement :: Parser Statement
_assign_statement = _ws >> do {
    v <- _id;
    _reserved "=";
    e <- _expr;
    return (Assign v Nothing e)
  }

_just_expr_statement :: Parser Statement
_just_expr_statement = _expr >>= \e -> return (JustExp e)

_statement :: Parser Statement
_statement =
              try(_assign_statement)
          <|> _just_expr_statement

_statements :: Parser [Statement]
_statements = endBy _statement _eos


errorAsMessages :: ParseError -> [String]
errorAsMessages theError = fmap messageString (errorMessages theError)

parseStatements :: String -> Either ParseError [Statement]
parseStatements = parse (_ws >> _statements <* eof) "(source)"
