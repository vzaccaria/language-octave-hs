{-# LANGUAGE OverloadedStrings #-}

module Grammar where

import           AST
import           Control.Applicative                 hiding (many, optional,
                                                      (<|>))
import           Data.Map.Strict
import           Expr
import           Lexer
import           Text.Parsec                         (runParserT)
import           Text.Parsec.Expr
import           Text.Parsec.Prim
import           Text.ParserCombinators.Parsec       hiding (try)
import           Text.ParserCombinators.Parsec.Error


_assign_statement :: DebugParse Statement
_assign_statement = _ws >> do {
    v <- _id;
    _reserved "=";
    e <- _expr;
    return (Assign v Nothing e)
  }

_just_expr_statement :: DebugParse Statement
_just_expr_statement = _expr >>= \e -> return (JustExp e)

_statement :: DebugParse Statement
_statement =
              try(_assign_statement)
          <|> _just_expr_statement

_statements :: DebugParse [Statement]
_statements = endBy _statement _eos


errorAsMessages :: ParseError -> [String]
errorAsMessages theError = fmap messageString (errorMessages theError)

parseStatements :: String -> IO (Either ParseError [Statement])
parseStatements = runParserT (_ws >> _statements <* eof) () "(source)"
