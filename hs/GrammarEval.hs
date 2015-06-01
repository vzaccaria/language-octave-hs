{-# LANGUAGE OverloadedStrings #-}



module GrammarEval where

import           AST
import           Control.Applicative
import           Control.Applicative.Lift
import           Control.Monad.Trans.State.Lazy
import           Data.Map.Strict                (empty, insert, lookup)
import           Data.Matrix
import           Debug.Trace
import           Eval
import           ExprEval
import           GHC.List
import           Grammar

type ProgramState = Either String Env
type ProgramStateProcessor = State ProgramState ()
type PureStateTransition = (ProgramState -> ProgramState)

addAnswer ::  MValue -> Eval ()
addAnswer valueV       = get >>= \e -> put $ insert "ans" valueV e

addVar :: String -> MValue -> Eval ()
addVar varnameV valueV = get >>= \e -> put $ insert varnameV valueV e

statementEval :: Statement -> Eval ()
statementEval (Assign varNameV Nothing expressionV) =  do {
    valueV <- (eeval expressionV);
    addAnswer valueV;
    addVar varNameV valueV;
}

statementEval (JustExp expressionV) = (eeval expressionV) >>= addAnswer

statementEvalTrampoline :: Statement -> ProgramStateProcessor
statementEvalTrampoline statementV = do {
    stateV  <- get;
    case stateV of
      (Right envV) ->
        let sEval = runEval3 envV (statementEval statementV) in
          case sEval of
            Left errorV -> put (Left errorV)
            Right ((), newSymTableV) -> put (Right newSymTableV)
      _ -> return ()
}

evalSList :: [Statement] -> Env -> ProgramState
evalSList statementListV initialSymTableV =
      let stateProcessors = fmap statementEvalTrampoline statementListV
          initialState = return initialSymTableV
          compSequence = foldl1 (seqTrans) stateProcessors in
          execState compSequence initialState where
            seqTrans accStateProc curStateV = modify (new . old) where
              old = (execState accStateProc)
              new = (execState curStateV)

showAnswerS :: Env -> String
showAnswerS symTableV = case (Data.Map.Strict.lookup "ans" symTableV) of
  (Just v) -> "\nans = \n" ++ (printMValue v)
  _ -> "ans = NA"

evalProgramIO :: String -> IO String
evalProgramIO x = return (evalProgram x)

evalProgram :: String -> String
evalProgram programString = outputString where
  (outputString, _) = evalSListS programString Data.Map.Strict.empty

evalSListS :: String -> Env -> (String, Env)

evalSListS programString initialSymTableV =
  case (parseStatements (programString ++ ";")) of
    Left err -> (("syntax error, " ++ (show err)), initialSymTableV);
    Right statementListV ->
      case evalSList statementListV initialSymTableV of
        Left errorV -> (("error, " ++ (errorV)), initialSymTableV)
        Right newSymTableV -> (showAnswerS newSymTableV, newSymTableV)
