{-# LANGUAGE OverloadedStrings #-}

module OctaveLexer (
  _id,
  _lexeme,
  _symbol,
  _char,
  _int,
  _stringLiteral
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Prim
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Functor.Identity


octaveDef :: GenLanguageDef String u Identity

octaveDef = emptyDef {
  Token.commentLine     = "%",

  Token.identStart      = letter,

  Token.identLetter     = alphaNum,

  Token.reservedNames   = [ "if" , "then" , "else" , "while" , "do",
                            "skip" , "end" , "true" , "false" ],

  Token.reservedOpNames = [   "+", "-", "*", "/", "=",
                              "~=", "==", "^", "./", ".*",
                              "<", ">", "<=", ">=",
                              "&", "&&", "|", "||" ]
}

lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser octaveDef

-- Derive your own microParsers to be combined here:

_id :: ParsecT String u Identity String
_id = Token.identifier lexer

_lexeme :: ParsecT String u Identity a -> ParsecT String u Identity a
_lexeme = Token.lexeme lexer

_symbol :: String -> ParsecT String u Identity String
_symbol = Token.symbol lexer

_char :: ParsecT String u Identity Char
_char = Token.charLiteral lexer

_int :: ParsecT String u Identity String
_int = do
        i <- Token.integer lexer
        return (show i)

_stringLiteral :: ParsecT String u Identity String
_stringLiteral = _lexeme $ between (_symbol "'") (_symbol "'" <?> "End of string found") (many _char)

-- stringLiteral = between (symbol "'") (symbol "'")
