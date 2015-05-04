{-# LANGUAGE OverloadedStrings #-}

module OctaveGrammarExpr where

import Text.Parsec.Expr
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Control.Applicative hiding (many, (<|>))
import Debug.Trace
import OctaveLexer
import PrettyPrint
import OctaveAST

_expr :: Parser Expr
_expr = buildExpressionParser table _term
      <|> _matrix
      <?> "expression error"

_term :: Parser Expr
_term = (_parens _expr)
  <|> _number_const_e

_number_const_e :: Parser Expr
_number_const_e = (try _float_const_e) <|> _int_const_e

_int_const_e :: Parser Expr
_int_const_e = ConstI <$> _int

_float_const_e :: Parser Expr
_float_const_e = ConstD <$> _double


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
