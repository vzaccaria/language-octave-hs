{-# LANGUAGE OverloadedStrings #-}



module GrammarEval where

import           AST
import           Control.Applicative
import           Control.Applicative.Lift
import           Control.Monad.Trans.State.Lazy
import           Data.Map.Strict                (empty, insert, lookup)
import           Data.Matrix
import           ExprEval
import           GHC.List
import           Grammar
import           Symtable

type ProgramState = Either String Symtable
type ProgramStateProcessor = State ProgramState ()
type PureStateTransition = (ProgramState -> ProgramState)

addAnswer :: MOD -> Symtable -> Symtable
addAnswer valueV symTableV = insert "ans" valueV symTableV

statementEval :: Statement -> Symtable -> Either String Symtable
statementEval (Assign varNameV Nothing expressionV) symTableV =
    case (eeval symTableV expressionV) of
      Left errorV -> (Left errorV)
      Right valueV -> (Right (insert varNameV valueV (addAnswer valueV symTableV)))

statementEval (JustExp expressionV) symTableV = do {
    ans <- eeval symTableV expressionV;
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

evalSList :: [Statement] -> Symtable -> ProgramState
evalSList statementListV initialSymTableV =
      let stateProcessors = fmap statementEvalTrampoline statementListV
          initialState = return initialSymTableV
          compSequence = foldl1 (seqTrans) stateProcessors in
          execState compSequence initialState where
            seqTrans accStateProc curStateV = modify (new . old) where
              old = (execState accStateProc)
              new = (execState curStateV)

showAnswerS :: Symtable -> String
showAnswerS symTableV = case (Data.Map.Strict.lookup "ans" symTableV) of
  (Just v) -> "\nans = \n" ++ (show v)
  _ -> "ans = NA"

evalProgramIO :: String -> IO String
evalProgramIO x = return (evalProgram x)

evalProgram :: String -> String
evalProgram programString = outputString where
  (outputString, _) = evalSListS programString Data.Map.Strict.empty

evalSListS :: String -> Symtable -> (String, Symtable)

evalSListS programString initialSymTableV =
  case (parseStatements (programString ++ ";")) of
    Left err -> (("syntax error, " ++ (show err)), initialSymTableV);
    Right statementListV ->
      case evalSList statementListV initialSymTableV of
        Left errorV -> (("error, " ++ (errorV)), initialSymTableV)
        Right newSymTableV -> (showAnswerS newSymTableV, newSymTableV)
