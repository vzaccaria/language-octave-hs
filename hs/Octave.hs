{-# LANGUAGE OverloadedStrings #-}

module Main where

import Haste.Foreign
import Haste.Prim (toJSStr)
import OctaveGrammar
import OctaveGrammarExpr

alive :: String -> IO String
alive _ = return "Hei, I am alive"

main :: IO ()
main = do
  export (toJSStr "alive") alive
  export (toJSStr "justParseStatements") justParse
  export (toJSStr "justParseExpression") justParseExpression
