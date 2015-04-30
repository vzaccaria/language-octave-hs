{-# LANGUAGE OverloadedStrings #-}

module OctaveLexer (
  _id,
  _lexeme,
  _symbol,
  _int,
  _stringLiteral,
  _ws,
  _eol,
  _eos
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Prim
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Functor.Identity
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Control.Applicative hiding (many, (<|>))


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
                              "<", ">", "<=", ">=", ":",
                              "&", "&&", "|", "||" ]
}

lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser octaveDef



-- Derive your own microParsers to be combined here:

_lexeme :: Parser String -> Parser String
_lexeme = Token.lexeme lexer

_id:: Parser String 
_id = many1 (alphaNum <|> char '_')

_symbol :: String -> Parser String
_symbol = Token.symbol lexer

_int :: Parser String
_int = do
        i <- Token.integer lexer
        return (show i)

_stringLiteral :: Parser String
_stringLiteral = _lexeme s
                 where
                   legitChar = noneOf [ '\'', '\n' ]
                   s = between (char '\'') (char '\'') (many legitChar)

_ws :: Parser ()
_ws = Token.whiteSpace lexer

_eol :: Parser Char
_eol = newline

_semi :: Parser Char
_semi = char ';'

_eos :: Parser Char
_eos = (_eol <|> _semi)
