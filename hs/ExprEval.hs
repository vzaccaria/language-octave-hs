{-# LANGUAGE OverloadedStrings #-}

module ExprEval where

import           AST
import           BuiltIns
import           Control.Monad.Error      hiding (sequence)
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import qualified Data.List                as List
import qualified Data.Map.Strict          as M (insert, lookup)
import           Data.Matrix              as X
import           Errors
import           Eval
import           ScalarNum
import           ScalarNumMat

eeval:: Expr -> Eval MValue

eeval (ConstI i)         = return $ s2m (In i)
eeval (ConstD f)         = return $ s2m (Do f)
eeval (ConstC c)         = return $ s2m (Co c)
eeval (Str st)           = return $ M (matrix 1 (length st) $ \(_,j) -> (Ch (st !! (j - 1))))
eeval (CTran e1)         = liftUnOp  (transpose . conj) (eeval e1)
eeval (Tran e1)          = liftUnOp  transpose (eeval e1)
eeval (BinOp "+" e1 e2)  = liftBinOp (+) (eeval e1) (eeval e2)
eeval (BinOp "-" e1 e2)  = liftBinOp (-) (eeval e1) (eeval e2)
eeval (BinOp "*" e1 e2)  = liftBinOp (*) (eeval e1) (eeval e2)
eeval (Unop  "-" e1)     = liftUnOp (scaleMatrix (In (-1))) (eeval e1)
eeval (Unop  "+" e1)     = eeval e1
eeval (BinOp _ _ _ )     = fail _eNotYetImplemented
eeval (Unop  _ _   )     = fail _eNotYetImplemented
eeval (Default)          = return DF

eeval (Row es) = List.foldl1 (liftBinOp (X.<|>)) dt
      where dt = fmap eeval es

eeval (Matrix es) = List.foldl1 (liftBinOp (X.<->)) dt
      where dt = fmap eeval es

eeval (Range a b) = do {
    (In i1) <- eeval a >>= m2i;
    (In i2) <- eeval b >>= m2i;
    let s1 = fromInteger (i2 - i1) in
      return $ M (matrix 1 (s1+1) $ \(_,j) -> (In (toInteger j + i1 - 1)))
} `catchError` (\_ -> fail _eLowLevelOperation)



eeval (EEval var args) = do {
  env <- get;
  case M.lookup var env of
    (Just x) -> evalThisSym x args where

      evalThisSym (L f) es = mapM eeval es >>= evalFunction f
      evalThisSym q     [] = return q
      evalThisSym _     _  = fail _eInvalidArguments

    (Nothing) -> fail $ _symbolNotFound var

} `catchError` (\_ -> fail _eLowLevelOperation)

-- assignVal :: String -> [Expr] -> Expr -> Env -> Env
-- assignVal name [] expr env = case (exprVal env expr) of
--     Right value -> M.insert name value env
--     Left err ->


exprVal :: Env -> Expr -> Either String (MValue, Env)
exprVal env ex = runEval3 env (eeval ex)

exprValToString :: Env -> Expr -> String
exprValToString symtable expr =
    case (runEval3 symtable (eeval expr)) of
    (Left err) -> "error: " ++ err
    (Right (v, _)) -> (show v)
