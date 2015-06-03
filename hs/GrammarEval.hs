{-# LANGUAGE OverloadedStrings #-}

module GrammarEval (evalSListS) where

import           AST
import           Control.Applicative
import           Control.Applicative.Lift
import           Control.Monad.Error
import           Control.Monad.Trans.State.Lazy
import           Data.Map.Strict                (empty, insert, lookup)
import           Data.Matrix
import           Debug.Trace
import           Errors
import           Eval
import           ExprEval
import           GHC.List
import           Grammar
import           Statement

type ProgramState          = Either String Env
type ProgramStateProcessor = State ProgramState ()
type PureStateTransition = (ProgramState -> ProgramState)

addAnswer ::  MValue -> Eval ()
addAnswer valueV = get >>= \e -> put $ insert "ans" valueV e


_evalToAnswer :: Statement -> Eval ()
_evalToAnswer s = do {
  mv <- statementEval s;
  addAnswer mv
}

statementEvalTrampoline :: Statement -> ProgramStateProcessor
statementEvalTrampoline statementV = do {
    stateV  <- get;
    case stateV of
      (Right envV) ->
        let sEval = runEval3 envV (_evalToAnswer statementV) in
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

unpackProgramState :: ProgramState -> Env -> (String, Env)
unpackProgramState newState oldSymTable = case newState of
  (Left s) -> (s, oldSymTable)
  (Right e) -> (showAnswerS e, e)


evalSListS :: String -> Env -> IO (String, Env)
evalSListS programString initialSymTableV = do {
    maybeStatementList <- parseStatements (programString ++ ";");
    case maybeStatementList of
      (Left _)               -> return $ (_eSyntaxError, initialSymTableV)
      (Right statementListV) -> return $ unpackedState where
                  unpackedState = unpackProgramState evaluationResult initialSymTableV
                  evaluationResult = (evalSList statementListV initialSymTableV) ;
  }
