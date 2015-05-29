{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Eval where

import           AST
import           Control.Applicative    hiding (empty)
import           Control.Monad.Error    hiding (sequence)
import           Control.Monad.Identity hiding (sequence)
import           Control.Monad.Reader   hiding (sequence)
import           Data.Complex
import           Data.List              as L
import           Data.Map               hiding (fromList, (!))
import           Data.Matrix
import           Data.Traversable
import           Errors
import           GHC.Base
import           Prelude                hiding (sequence)
import           PrettyPrint
import           ScalarNum
import           ScalarNumMat
import qualified Text.PrettyPrint.Boxes as B

data Value        = I Integer | D Double | O (Complex Double) | C Char  deriving Show
data MValue       = M (Matrix Value) | L Lambda | DF deriving Show

type Env = Map String MValue

data Lambda =
  Lam [String] [String] [Statement]
  | BuiltInOp1 (MValue -> Eval MValue)
  | BuiltInOp2 (MValue -> MValue -> Eval MValue)

instance Show Lambda where
  show (Lam p1 p2 _) = "\\" ++ (vectorAsArgs p1) ++ " -> " ++ (vectorAsArgs p2)
  show (BuiltInOp2 _) = "built-in (2 arguments)"
  show (BuiltInOp1 _) = "built-in (1 argument)"

type Eval a = ReaderT Env (ErrorT String Identity) a

runEval3 :: Env -> Eval a -> Either String a
runEval3 env ev = runIdentity (runErrorT (runReaderT ev env))


-- Num instance for ScalarNum
liftUnOp :: (NumMat -> NumMat) -> Eval MValue -> Eval MValue
liftUnOp op v1 = do {
  vv1 <- v1;
  a1 <- toNumMat vv1;
  (fromNumMat (op a1))
} `catchError` (\_ -> (fail _eLowLevelOperation))


-- Num instance for ScalarNum
liftBinOp :: (NumMat -> NumMat -> NumMat) -> Eval MValue -> Eval MValue -> Eval MValue
liftBinOp op v1 v2 = do {
  vv1 <- v1;
  vv2 <- v2;
  a1 <- toNumMat vv1;
  a2 <- toNumMat vv2;
   (fromNumMat (op a1 a2))
} `catchError` (\_ -> (fail _eLowLevelOperation))

liftEwiseOp :: (ScalarNum -> ScalarNum) -> Eval MValue -> Eval MValue
liftEwiseOp op v1 = do {
  vv1 <- v1;
  a1 <- toNumMat vv1;
  (fromNumMat (fmap op a1))
} `catchError` (\_ -> (fail _eLowLevelOperation))


printMValue :: MValue -> String
printMValue m =
    let nm = (toNumMat m)
        x = runEval3 empty nm in
    case x of
      (Right q) -> (B.render (buildBoxMatrix q))
      _ -> _eInvalidArguments

--- Low level injections to/from NumMat

toNum :: Value -> Eval ScalarNum
toNum x = case x of
  (I i1) -> return (In i1)
  (D d1) -> return (Do d1)
  (C c1) -> return (Ch c1)
  (Eval.O c1) -> return (Co c1)

fromNum :: ScalarNum -> Value
fromNum x = case x of
  (In i1) -> (I i1)
  (Do d1) -> (D d1)
  (Ch c1) -> (C c1)
  (Co c) -> (Eval.O c)


toNumMat :: MValue -> Eval NumMat
toNumMat (M v1) = sequence r
    where n = nrows v1
          m = ncols v1
          r = matrix n m (\(i,j) -> (toNum (v1 ! (i,j))))
toNumMat _ = fail _eInvalidArguments


fromNumMat :: NumMat -> Eval MValue
fromNumMat v1 = return r
    where n = nrows v1
          m = ncols v1
          r = M (matrix n m (\(i,j) -> (fromNum (v1 ! (i,j)))))


getEl :: MValue -> (Int, Int) -> Eval Value
getEl (M m) (i1, i2) = do {
  case (i1 > (nrows m), i2 > (ncols m)) of
    (False, False) -> return $ m ! (i1, i2)
    _ -> fail "Index out of bound"
}
getEl _ _ = fail "_eInvalidArguments"

--
