{-# LANGUAGE OverloadedStrings #-}

module Repl where

import           AST
import qualified Control.Exception        as E
import           Data.Map.Strict
import           Expr
import           ExprEval
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

printResult :: Symtable -> Expr -> String
printResult symtable expr =
    case (eeval symtable expr) of
    (Left err) -> "error: " ++ err
    (Right v) -> (show v)

evalString :: String -> Symtable -> InputT IO ()
evalString ss symtable = do {
  case (parseExpression ss) of
    Left err ->
      outputStrLn ("syntax error, " ++ (show err));
    Right expr ->
      outputStrLn ("\nans = \n" ++ (printResult symtable expr))
}

loop :: ReplState -> InputT IO ()
loop (R step symtable) = do
  s <- getInputLine ("minioctave:" ++ (show step) ++ "> ")
  case s of
    Nothing -> loop (R (step + 1) symtable)
    Just "exit" -> return ()
    Just "quit" -> return ()
    Just ss -> do {
      evalString ss symtable;
      loop (R (step + 1) symtable);
    }

repl :: IO ()
repl = runInputT defaultSettings (loop (R 0 empty))
