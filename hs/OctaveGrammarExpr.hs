{-# LANGUAGE OverloadedStrings #-}

module OctaveGrammarExpr where

import Text.Parsec.Expr
import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, (<|>))
import Debug.Trace
import OctaveLexer
import OctaveAST
import OctaveASTPPr

_expr :: Parser Expr
_expr = buildExpressionParser table _term
      <|> _matrix
      <|> _default
      <?> "expression error"

_eval_sym :: Parser Expr
_eval_sym = do { v <- _id; return (Eval v []) }

_eval_indexed_sym :: Parser Expr
_eval_indexed_sym = do { v <- _id;
                 elist <-  _parens( sepBy _expr (_reserved ","));
                 return (Eval v elist) }

_string:: Parser Expr
_string = (Str <$> _stringLiteral)

_term :: Parser Expr
_term =
            (_parens _expr)
        <|> _number
        <|> try (_eval_indexed_sym)
        <|> _eval_sym
        <|> _string


_number :: Parser Expr
_number = (try _float_const_e) <|> _int_const_e

_int_const_e :: Parser Expr
_int_const_e = ConstI <$> _int

_float_const_e :: Parser Expr
_float_const_e = ConstD <$> _double

_default:: Parser Expr
_default = do { _reserved ":"; return Default }

_vector :: Parser Expr
_vector = Row <$> (many _expr)

_matrix :: Parser Expr
_matrix = Matrix <$> _brackets (sepBy _vector (_reserved ";"))

-- From: https://hackage.haskell.org/package/parsec-3.0.0/docs/Text-Parsec-Expr.html

table   = [   [
                prefix "+" (Unop "+"),
                prefix "-" (Unop "-") ],

              [
                binary "*" (BinOp "*") AssocLeft,
                binary "/" (BinOp "/") AssocLeft ],

              [
                binary "+" (BinOp "+") AssocLeft,
                binary "-" (BinOp "-") AssocLeft ],

              [
                binary ":" (Cons) AssocLeft ]
            ]

binary  name fun assoc = Infix    (do{ _reserved name; return fun }) assoc
prefix  name fun       = Prefix   (do{ _reserved name; return fun })
postfix name fun       = Postfix  (do{ _reserved name; return fun })

t x = trace ("value: " ++ (show x)) x

parseExpression :: String -> Either ParseError Expr
parseExpression = parse (_ws >> _expr <* eof) "(source)"

justParseExpression :: String -> IO String
justParseExpression s = return $
          case parsed of
            Right value -> "ok: " ++ (show value)
            Left _ -> "error"
          where parsed = parseExpression s


justTranslateExpression :: String -> IO String
justTranslateExpression s = return $
              case parsed of
                Right value -> "ok: " ++ (prExpr value)
                Left _ -> "error"
              where parsed = parseExpression s
