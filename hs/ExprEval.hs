{-# LANGUAGE OverloadedStrings #-}

module ExprEval where

import           AST
import           BuiltIns
import           Control.Monad.Reader
import           Data.Complex
import qualified Data.List            as List
import qualified Data.Map.Strict      as M (empty, lookup)
import           Data.Matrix          as X
import           Errors
import           Eval
import           Expr


eeval:: Expr -> Eval MValue

eeval (ConstI i)        = return $ (fromList 1 1 [ I i ])
eeval (ConstD f)        = return $ (fromList 1 1 [ D f ])
eeval (ConstC c)        = return $ (fromList 1 1 [ O c])
eeval (Str st)          = return $ (matrix 1 (length st) $ \(_,j) -> (C (st !! (j - 1))))

eeval (CTran e1)         = liftUnOp  (transpose . conj) (eeval e1)
eeval (Tran e1)          = liftUnOp  (transpose) (eeval e1)
eeval (BinOp "+" e1 e2)  = liftBinOp (+) (eeval e1) (eeval e2)
eeval (BinOp "-" e1 e2)  = liftBinOp (-) (eeval e1) (eeval e2)
eeval (BinOp "*" e1 e2)  = liftBinOp (*) (eeval e1) (eeval e2)
eeval (Unop  "-" e1)     = liftUnOp (scaleMatrix (In (-1))) (eeval e1)
eeval (Unop  "+" e1)     = (eeval e1)


eeval (Row es) = List.foldl1 (liftBinOp (X.<|>)) dt
      where dt = (fmap (eeval) es)

eeval (Matrix es) = List.foldl1 (liftBinOp (X.<->)) dt
      where dt = (fmap (eeval) es)

eeval (Range a b) = do {
    a1 <- eeval a;
    b1 <- eeval b;
    (I i1) <- getEl (a1) (1,1);
    (I i2) <- getEl (b1) (1,1);
    let s1 = fromInteger (i2 - i1) in
      return $ (matrix 1 (s1+1) $ \(_,j) -> (I ((toInteger j) + i1 - 1)))
}



-- Ok, lets start with the boogie here..
eeval (EEval var []) = do {
  env <- ask;
  case (M.lookup var env) of
    (Just x) -> return x
    (Nothing) -> fail $ _symbolNotFound var
}

eeval (EEval symbol es) = do {
  env   <- ask;
  case (M.lookup symbol env) of
    (Just mval) -> do {
      (L f)   <- getEl mval (1,1);
      dt      <- sequence (map (eeval) es);
      evalFunction f dt
    }
    _ -> fail _eNotYetImplemented
}


exprVal :: Env -> Expr -> Either String MValue
exprVal env ex = runEval3 env (eeval ex)

exprValToString :: Env -> Expr -> String
exprValToString symtable expr =
    case (runEval3 symtable (eeval expr)) of
    (Left err) -> "error: " ++ err
    (Right v) -> (show v)
