{-# LANGUAGE OverloadedStrings #-}

module Expr where

import           AST
import           Control.Applicative           hiding (many, optional, (<|>))
import           Debug.Trace
import           Lexer
import           Text.Parsec                   (runParserT)
import           Text.Parsec.Expr
import           Text.Parsec.Prim
import           Text.ParserCombinators.Parsec hiding (try)

t x = trace ("value: " ++ (show x)) x

opTable = buildExpressionParser table

_expr :: DebugParse Expr
_expr =   opTable _primary_expression
      <|>  _string
      <?> "expression error"

_primary_expression:: DebugParse Expr
_primary_expression =
                  try (_eval_indexed_sym)
                  <|> try (_eval_sym)
                  <|> try (_number)
                  <|> (_parens _expr)
                  <|> _matrix
                  <|> _default

_eval_sym :: DebugParse Expr
_eval_sym = do { v <- _id; return (EEval v []) }

_eval_indexed_sym :: DebugParse Expr
_eval_indexed_sym = do { v <- _id;
                 elist <-  _parens( _commaSep _expr );
                 return (EEval v elist) }

_string:: DebugParse Expr
_string = (Str <$> _stringLiteral)

_number :: DebugParse Expr
_number =
        (try _complex_const_e)
    <|> (try _float_const_e)
    <|> (try _int_const_e)

_int_const_e :: DebugParse Expr
_int_const_e = ConstI <$> _int

_float_const_e :: DebugParse Expr
_float_const_e = ConstD <$> _double

_complex_const_e :: DebugParse Expr
_complex_const_e = ConstC <$> ((try _imaginary_d) <|> (try _imaginary_i))

_default:: DebugParse Expr
_default = do { __default ; return Default }

_vector :: DebugParse Expr
_vector = Row <$> (sepBy1 _expr (optional (char ',')))

_matrix :: DebugParse Expr
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

              [   postfix "'" (CTran),
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


parseExpression :: String -> IO (Either ParseError Expr)
parseExpression r = runParserT (_ws >> _expr <* eof) () "(source)" r


justParseExpression :: String -> IO String
justParseExpression s = do {
        parsed <- parseExpression s;
        case parsed of
          (Right value) -> return $ "ok: " ++ (show value)
          (Left e) -> return $ show e
  }
