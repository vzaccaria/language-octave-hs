{-# LANGUAGE OverloadedStrings #-}

module Main where

import Haste.Foreign
import Haste.Prim (toJSStr)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Token
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Functor.Identity
import Text.ParserCombinators.Parsec.Error
import OctaveLexer



-- The whole parser
theParser :: ParsecT String u Identity String
theParser = _id <|> _int <|> _stringLiteral



-- Utility function
parseIt :: String -> Either ParseError String
parseIt = parse theParser "(source)"

alive :: String -> IO String
alive _ = return "Hei, I am alive"

justLex :: String -> IO String
justLex s = return $
          case parsed of
            Right value -> "It works! just lexed: " ++ value
            Left theError -> let
              errorString = foldr (++) "" (fmap messageString (errorMessages theError))
              in "Just got: " ++ errorString
          where parsed = parseIt s


main :: IO ()
main = do
  export (toJSStr "alive") alive
  export (toJSStr "justLex") justLex
