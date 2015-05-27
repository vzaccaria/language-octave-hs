{-# LANGUAGE OverloadedStrings #-}

module Expr where

import           AST
import           Control.Applicative           hiding (many, optional, (<|>))
import           Debug.Trace
import           Lexer
import           Text.Parsec.Expr
import           Text.ParserCombinators.Parsec

t x = trace ("value: " ++ (show x)) x

opTable = buildExpressionParser table

_expr :: Parser Expr
_expr =   opTable _primary_expression
      <|>  _string
      <?> "expression error"

_primary_expression:: Parser Expr
_primary_expression =
                  try (_eval_indexed_sym)
                  <|> try (_eval_sym)
                  <|> try (_number)
                  <|> (_parens _expr)
                  <|> _matrix
                  <|> _default

_eval_sym :: Parser Expr
_eval_sym = do { v <- _id; return (EEval v []) }

_eval_indexed_sym :: Parser Expr
_eval_indexed_sym = do { v <- _id;
                 elist <-  _parens( sepBy _expr (_reserved ","));
                 return (EEval v elist) }

_string:: Parser Expr
_string = (Str <$> _stringLiteral)

_number :: Parser Expr
_number =
        (try _complex_const_e)
    <|> (try _float_const_e)
    <|> (try _int_const_e)

_int_const_e :: Parser Expr
_int_const_e = ConstI <$> _int

_float_const_e :: Parser Expr
_float_const_e = ConstD <$> _double

_complex_const_e :: Parser Expr
_complex_const_e = ConstC <$> ((try _imaginary_d) <|> (try _imaginary_i))

_default:: Parser Expr
_default = do { _reserved ":"; return Default }

_vector :: Parser Expr
_vector = Row <$> (sepBy1 _expr (optional (char ',')))

_matrix :: Parser Expr
_matrix = Matrix <$> _brackets (sepBy _vector (_reserved ";"))

-- From: https://hackage.haskell.org/package/parsec-3.0.0/docs/Text-Parsec-Expr.html

table   = [
              [
                binary "**" (BinOp "**") AssocLeft ,
                binary ".**" (BinOp ".**") AssocLeft,
                binary "^" (BinOp "^") AssocLeft ,
                binary ".^" (BinOp ".^") AssocLeft
                  ],

              [
                prefix "+" (Unop "+"),
                prefix "-" (Unop "-")],

              [  postfix "'" (CTran),
                postfix ".'" (Tran) ],

              [
                binary "*" (BinOp "*") AssocLeft,
                binary ".*" (BinOp ".*") AssocLeft,
                binary "/" (BinOp "/") AssocLeft ,
                binary "./" (BinOp "./") AssocLeft,
                binary "\\" (BinOp "\\") AssocLeft ,
                binary ".\\" (BinOp ".\\") AssocLeft
                 ],
              [
                binary "+" (BinOp "+") AssocLeft,
                binary "-" (BinOp "-") AssocLeft,
                binary ".+" (BinOp ".+") AssocLeft,
                binary ".-" (BinOp ".-") AssocLeft ],

              [
                binary ":" (Range) AssocLeft ]

              -- Relational operators here..
              -- See http://www.chemie.fu-berlin.de/chemnet/use/info/octave/octave_4.html#SEC41
            ]

binary  name fun assoc = Infix    (do{ _reserved name; return fun }) assoc
prefix  name fun       = Prefix   (do{ _reserved name; return fun })
postfix name fun       = Postfix  (do{ _reserved name; return fun })


parseExpression :: String -> Either ParseError Expr
parseExpression = parse (_ws >> _expr <* eof) "(source)"


justParseExpression :: String -> IO String
justParseExpression s = return $
          case parsed of
            Right value -> "ok: " ++ (show value)
            Left e -> show e
          where parsed = parseExpression s
