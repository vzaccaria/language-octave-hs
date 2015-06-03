{-# LANGUAGE OverloadedStrings #-}

module Repl where

import           AST
import           Browser
import           BuiltIns
import qualified Control.Exception        as E
import           Control.Monad.IO.Class
import           Data.Map.Strict
import           Eval
import           Expr
import           ExprEval
import           GrammarEval
import           System.Console.Haskeline
import           Text.Printf


data ReplState = R {
  _step     :: Int,
  _symtable :: Env
}



evalSListIOBrowser :: String ->  Env -> IO Env
evalSListIOBrowser programString initialSymTableV = E.catch action handler
  where
      action = do {
          (message, symTableV) <- evalSListS programString initialSymTableV;
          printBrowser message;
          return symTableV
      }
      handler :: SomeException -> IO Env
      handler = (\e -> do { _ <- printBrowser (show e); return initialSymTableV })

loopBrowser:: (String ->  Env -> IO  Env) -> ReplState -> IO ()
loopBrowser eio (R step symtable) = do
  s <- askBrowser
  case s of
    "exit" -> return ()
    "quit" -> return ()
    ss -> do {
      newSymTableV <- eio ss symtable;
      loopBrowser eio (R (step + 1) newSymTableV);
    }

loop:: (String ->  Env -> InputT IO  Env) -> ReplState -> InputT IO ()
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



evalSListIO :: String ->  Env -> InputT IO Env
evalSListIO programString initialSymTableV = do {
  (message, symTableV) <- liftIO (evalSListS programString initialSymTableV);
  outputStrLn message;
  return symTableV
}

evalExprIO :: String ->  Env -> InputT IO Env
evalExprIO ss symtable = do {
  ps <- liftIO (parseExpression ss);
  case ps of
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
