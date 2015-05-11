{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Expr
import           Grammar
import           Haste.Foreign
import           Haste.Prim    (toJSStr)

alive :: String -> IO String
alive _ = return "Hei, I am alive"

main :: IO ()
main = do
  export (toJSStr "alive") alive
  export (toJSStr "justParseStatements") justParse
  export (toJSStr "justParseExpression") justParseExpression
