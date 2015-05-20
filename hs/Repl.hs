{-# LANGUAGE OverloadedStrings #-}

module Repl where

import           AST
import           Browser
import qualified Control.Exception        as E
import           Data.Map.Strict
import           Expr
import           ExprEval
import           Function
import           GrammarEval
import           Symtable
import           System.Console.Haskeline
import           Text.Printf




data ReplState = R {
  _step     :: Int,
  _symtable :: Symtable
}



evalSListIOBrowser :: String -> Symtable -> IO Symtable
evalSListIOBrowser programString initialSymTableV = E.catch action handler
  where
      action = do {
          _ <- printBrowser message;
          return symTableV
          }
      (message, symTableV) = evalSListS programString initialSymTableV
      handler :: SomeException -> IO Symtable
      handler = (\e -> do { _ <- printBrowser (show e); return initialSymTableV })

loopBrowser:: (String -> Symtable -> IO Symtable) -> ReplState -> IO ()
loopBrowser eio (R step symtable) = do
  s <- askBrowser
  case s of
    "exit" -> return ()
    "quit" -> return ()
    ss -> do {
      newSymTableV <- eio ss symtable;
      loopBrowser eio (R (step + 1) newSymTableV);
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


replSListBrowser :: IO ()
replSListBrowser = loopBrowser evalSListIOBrowser (R 0 initialSymTable)

replExp :: IO ()
replExp = runInputT defaultSettings (loop evalExprIO (R 0 initialSymTable))

replSList :: IO ()
replSList = runInputT defaultSettings (loop evalSListIO (R 0 initialSymTable))
