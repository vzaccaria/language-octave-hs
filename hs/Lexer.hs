{-# LANGUAGE OverloadedStrings #-}

module Lexer  where

import           Data.Complex
import           Data.Functor.Identity
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

-- We start from a classic definition for tokens in our language
octaveDef :: GenLanguageDef String u Identity
octaveDef = emptyDef {
  Token.commentLine     = "%",

  Token.identStart      = letter,

  Token.identLetter     = alphaNum,

  Token.reservedNames   = [ "if" , "then" , "else" , "while" , "do",
                            "skip" , "end" , "true" , "false" ],

  Token.reservedOpNames = [   "+", "-", "*", "/", "=",
                              "~=", "==", "^", "./", ".*",
                              "<", ">", "<=", ">=", ":", "'", "i",
                              "&", "&&", "|", "||" ]
}

lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser octaveDef

-- Derive your own microParsers to be combined here:

_lexeme:: Parser a -> Parser a
_lexeme = Token.lexeme lexer


_id:: Parser String
_id = _lexeme $ do {
        c <- letter <|> char '_';
        cs <- many (alphaNum <|> char '_');
        return (c:cs)
        }

_symbol :: String -> Parser String
_symbol = Token.symbol lexer

_int :: Parser Integer
_int = _lexeme (Token.integer lexer)

_double :: Parser Double
_double = _lexeme (Token.float lexer)

_imaginary_d :: Parser (Complex Double)
_imaginary_d = do {
  dd <- _double;
  _ <- _reserved "i";
  return (0 :+ dd)
}

_imaginary_i :: Parser (Complex Double)
_imaginary_i = do {
  dd <- _int;
  _ <- _reserved "i";
  return (0 :+ (fromInteger dd))
}

_stringLiteral :: Parser String
_stringLiteral = _lexeme s
                 where
                   legitChar = noneOf [ '\'', '\n' ]
                   s = between (char '\'') (char '\'') (many legitChar)

_reserved:: String -> Parser ()
_reserved = Token.reservedOp lexer

_ws :: Parser ()
_ws = Token.whiteSpace lexer

_eol :: Parser Char
_eol = newline

_parens:: Parser a -> Parser a
_parens = Token.parens lexer

_brackets:: Parser a -> Parser a
_brackets = Token.brackets lexer

_semi :: Parser Char
_semi = char ';'

_eos :: Parser Char
_eos = (_eol <|> _semi)
