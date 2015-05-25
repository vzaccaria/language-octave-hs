{-# LANGUAGE OverloadedStrings #-}



module GrammarEval where

import           AST
import           Control.Applicative
import           Control.Applicative.Lift
import           Control.Monad.Trans.State.Lazy
import           Data.Map.Strict                (empty, insert, lookup)
import           Data.Matrix
import           Eval
import           ExprEval
import           GHC.List
import           Grammar

type ProgramState = Either String Env
type ProgramStateProcessor = State ProgramState ()
type PureStateTransition = (ProgramState -> ProgramState)

addAnswer :: MValue -> Env -> Env
addAnswer valueV symTableV = insert "ans" valueV symTableV

statementEval :: Statement -> Env -> Either String Env
statementEval (Assign varNameV Nothing expressionV) symTableV =
    case (exprVal symTableV expressionV) of
      Left errorV -> (Left errorV)
      Right valueV -> (Right (insert varNameV valueV (addAnswer valueV symTableV)))

statementEval (JustExp expressionV) symTableV = do {
    ans <- exprVal symTableV expressionV;
    Right (addAnswer ans symTableV)
  }

statementEvalTrampoline :: Statement -> ProgramStateProcessor
statementEvalTrampoline statementV = do {
    stateV <- get;
    case stateV of
      Left errorV -> put (Left errorV)
      Right symTableV -> do {
        case (statementEval statementV symTableV) of
          Left errorV -> put (Left errorV)
          Right newSymTableV -> put (Right newSymTableV)
      }
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
