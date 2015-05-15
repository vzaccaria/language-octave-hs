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

showAnswer :: Symtable -> InputT IO ()
showAnswer symTableV = outputStrLn (showAnswerS symTableV)

evalSListIO :: String -> Symtable -> InputT IO Symtable
evalSListIO programString initialSymTableV = do {
  outputStrLn message;
  return symTableV;
  } where
    (message, symTableV) = evalSListS programString initialSymTableV

evalExprIO :: String -> Symtable -> InputT IO Symtable
evalExprIO ss symtable = do {
  case (parseExpression ss) of
    Left err -> do {
      outputStrLn ("syntax error, " ++ (show err));
      return symtable
    }
    Right expr -> do {
      outputStrLn ("\nans = \n" ++ (exprValToString symtable expr));
      return symtable
    }
}


replExp :: IO ()
replExp = runInputT defaultSettings (loop evalExprIO (R 0 empty))

replSList :: IO ()
replSList = runInputT defaultSettings (loop evalSListIO (R 0 empty))
