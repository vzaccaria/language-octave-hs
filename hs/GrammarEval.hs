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
import           System.Console.Haskeline

type ProgramState = Either String Symtable
type ProgramStateProcessor = State ProgramState ()
type PureStateTransition = (ProgramState -> ProgramState)

addAns :: MOD -> Symtable -> Symtable
addAns valueV symTableV = insert "ans" valueV symTableV

statementEval :: Statement -> Symtable -> Either String Symtable
statementEval (Assign varNameV Nothing expressionV) symTableV =
    case (eeval symTableV expressionV) of
      Left errorV -> (Left errorV)
      Right valueV -> (Right (insert varNameV valueV (addAns valueV symTableV)))

statementEval (JustExp expressionV) symTableV = do {
    ans <- eeval symTableV expressionV;
    Right (addAns ans symTableV)
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

showAnswer :: Symtable -> InputT IO ()
showAnswer symTableV = case (Data.Map.Strict.lookup "ans" symTableV) of
  (Just v) -> outputStrLn ("\nans = \n" ++ (show v))
  _ -> outputStr "ans = NA"

evalSListIO :: String -> Symtable -> InputT IO Symtable
evalSListIO programString initialSymTableV = do {
  case (parseStatements (programString ++ ";")) of
    Left err -> do {
      outputStrLn ("syntax error, " ++ (show err));
      return initialSymTableV
    }
    Right statementListV -> do {
      case evalSList statementListV initialSymTableV of
        Left errorV -> do {
          outputStrLn ("error, " ++ (errorV));
          return initialSymTableV
        }
        Right newSymTableV -> do {
          showAnswer newSymTableV;
          return newSymTableV
        }
    }
}
