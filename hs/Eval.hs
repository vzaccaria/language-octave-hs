{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Eval where

import           AST
import           Control.Applicative      hiding (empty)
import           Control.Monad.Error      hiding (sequence)
import           Control.Monad.Identity   hiding (sequence)
import           Control.Monad.Reader     hiding (sequence)
import           Control.Monad.State.Lazy
import           Data.Complex
import           Data.List                as L
import           Data.Map                 hiding (fromList, (!))
import           Data.Matrix
import           Data.Traversable
import           Errors
import           GHC.Base
import           Prelude                  hiding (sequence)
import           PrettyPrint
import           ScalarNum
import           ScalarNumMat
import qualified Text.PrettyPrint.Boxes   as B

data MValue       = M NumMat | L Lambda | DF deriving Show

type Env = Map String MValue

data Lambda =
  Lam [String] [String] [Statement]
  | BuiltInOp1 (MValue -> Eval MValue)
  | BuiltInOp2 (MValue -> MValue -> Eval MValue)

instance Show Lambda where
  show (Lam p1 p2 _) = "\\" ++ (vectorAsArgs p1) ++ " -> " ++ (vectorAsArgs p2)
  show (BuiltInOp2 _) = "built-in (2 arguments)"
  show (BuiltInOp1 _) = "built-in (1 argument)"

type Eval a = StateT Env (ErrorT String Identity) a

runEval3 :: Env -> Eval a -> Either String (a, Env)
runEval3 env ev = runIdentity (runErrorT (runStateT ev env))


liftUnOp :: (NumMat -> NumMat) -> Eval MValue -> Eval MValue
liftUnOp op v1 = do {
  (M a1) <- v1;
  return (M (op a1))
} `catchError` (\_ -> fail _eLowLevelOperation)


liftBinOp :: (NumMat -> NumMat -> NumMat) -> Eval MValue -> Eval MValue -> Eval MValue
liftBinOp op v1 v2 = do {
  (M a1) <- v1;
  (M a2) <- v2;
  return (M (op a1 a2))
} `catchError` (\_ -> fail _eLowLevelOperation)



printMValue :: MValue -> String
printMValue nm =
    let x = runEval3 empty (return nm) in
    case x of
      (Right (M q, _)) -> (B.render (buildBoxMatrix q))
      _ -> _eInvalidArguments

s2m :: ScalarNum -> MValue
s2m v = M (matrix 1 1 (\(_,_) -> v))

m2s :: MValue -> Eval ScalarNum
m2s (M a1) = getEl (M a1) (1,1);
m2s _ = fail _eInvalidArguments

m2i :: MValue -> Eval ScalarNum;
m2i m = do {
  (In x) <- m2s m;
  return (In x)
} `catchError` (\_ -> fail _eInvalidArguments)

getEl :: MValue -> (Int, Int) -> Eval ScalarNum
getEl (M m) (i1, i2) = do {
  case (i1 > (nrows m), i2 > (ncols m)) of
    (False, False) -> return $ m ! (i1, i2)
    _ -> fail "Index out of bound"
}
getEl _ _ = fail _eInvalidArguments


asVector :: NumMat -> Eval MValue
asVector m = return $ (M (colVector (getMatrixAsVector m)))

evalMatrixAccess :: MValue -> [MValue] -> Eval MValue
evalMatrixAccess m        []            = return m
evalMatrixAccess (M m)    (DF:[]   )    = asVector m
evalMatrixAccess n@(M _)  (DF:DF:[])    = return n
evalMatrixAccess _        _             = fail _eInvalidArguments
