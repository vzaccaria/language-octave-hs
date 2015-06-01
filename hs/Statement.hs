

module Statement where

import           AST
import           Control.Monad.Trans.State.Lazy
import           Data.Map.Strict
import           Eval
import           ExprEval

statementEval :: Statement -> Eval MValue
statementEval (Assign varNameV Nothing expressionV) = eeval expressionV >>= addVar varNameV
statementEval (JustExp expressionV)                 = eeval expressionV

addVar :: String -> MValue -> Eval MValue
addVar varnameV valueV = do {
  e <- get;
  put (insert varnameV valueV e);
  return valueV
}
