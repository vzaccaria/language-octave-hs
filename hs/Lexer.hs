{-# LANGUAGE OverloadedStrings #-}

module Lexer  where

import           Control.Monad.IO.Class
import           Data.Complex
import           Data.Functor.Identity
import           Text.Parsec
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

type DebugParse a       = ParsecT String () IO a
type DebugLanguageDef a = Token.GenLanguageDef String a IO
type DebugTokenParser a = Token.GenTokenParser String a IO

-- We start from a classic definition for tokens in our language
octaveDef :: DebugLanguageDef a
octaveDef = Token.LanguageDef {
  Token.commentLine     = "%",
  Token.commentStart    = "",
  Token.commentEnd      = "",
  Token.nestedComments  = False,

  Token.caseSensitive   = True,

  Token.identStart      = letter,
  Token.identLetter     = alphaNum,

  Token.reservedNames   = [ "if" , "then" , "else" , "while" , "do",
                            "skip" , "end" , "true" , "false", ":" ],

  Token.opStart         = oneOf "!#$%&*+./<=>?@\\^|-~",
  Token.opLetter        = oneOf "!#$%&*+./<=>?@\\^|-~",

  Token.reservedOpNames = [   "+", "-", "*", "/", "=",
                              "~=", "==", "^", "./", ".*",
                              "<", ">", "<=", ">=", "'", "i",
                              "&", "&&", "|", "||" ]
}

lexer :: DebugTokenParser a
lexer = Token.makeTokenParser octaveDef

-- Derive your own microParsers to be combined here:

_lexeme:: DebugParse a -> DebugParse a
_lexeme = Token.lexeme lexer

__id :: DebugParse String
__id = _lexeme $ do {
        c <- letter <|> char '_';
        cs <- many (alphaNum <|> char '_');
        return (c:cs)
        }


_id:: DebugParse String
_id = __id

_symbol :: String -> DebugParse String
_symbol = Token.symbol lexer

_int :: DebugParse Integer
_int = _lexeme (Token.integer lexer)

_double :: DebugParse Double
_double = _lexeme (Token.float lexer)

_imaginary_d :: DebugParse (Complex Double)
_imaginary_d = do {
  dd <- _double;
  _ <- _reserved "i";
  return (0 :+ dd)
}

_imaginary_i :: DebugParse (Complex Double)
_imaginary_i = do {
  dd <- _int;
  _ <- _reserved "i";
  return (0 :+ (fromInteger dd))
}

_stringLiteral :: DebugParse String
_stringLiteral = _lexeme s
                 where
                   legitChar = noneOf [ '\'', '\n' ]
                   s = between (char '\'') (char '\'') (many legitChar)

_reserved:: String -> DebugParse ()
_reserved = do {
  Token.reservedOp lexer;
}

_ws :: DebugParse ()
_ws = Token.whiteSpace lexer

_eol :: DebugParse Char
_eol = newline

_parens:: DebugParse a -> DebugParse a
_parens = Token.parens lexer

_brackets:: DebugParse a -> DebugParse a
_brackets = Token.brackets lexer

_semi :: DebugParse Char
_semi = char ';'

__default :: DebugParse ()
__default = Token.colon lexer >> return ()

_commaSep = Token.commaSep lexer

_eos :: DebugParse Char
_eos = (_eol <|> _semi)
