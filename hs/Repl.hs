{-# LANGUAGE OverloadedStrings #-}

module Repl where

import           AST
import qualified Control.Exception        as E
import           Data.Map.Strict
import           Expr
import           ExprEval
import           GrammarEval
import           Symtable
import           System.Console.Haskeline
import           Text.Printf




data ReplState = R {
  _step     :: Int,
  _symtable :: Symtable
}

prompt :: Int -> IO String
prompt i = do {
    printf "%d > " i;
    getLine
  }

loop:: (String -> Symtable -> InputT IO Symtable) -> ReplState -> InputT IO ()
loop eio (R step symtable) = do
  s <- getInputLine ("minioctave:" ++ (show step) ++ "> ")
  case s of
    Nothing -> loop eio (R (step + 1) symtable)
    Just "exit" -> return ()
    Just "quit" -> return ()
    Just ss -> do {
      newSymTableV <- eio ss symtable;
      loop eio (R (step + 1) newSymTableV);
    }

replExp :: IO ()
replExp = runInputT defaultSettings (loop evalExprIO (R 0 empty))

replSList :: IO ()
replSList = runInputT defaultSettings (loop evalSListIO (R 0 empty))
