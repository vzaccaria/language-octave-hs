{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval where

import           AST
import           Control.Monad.Error    hiding (sequence)
import           Control.Monad.Identity hiding (sequence)
import           Control.Monad.Reader   hiding (sequence)
import           Data.List              as L
import           Data.Map               hiding (fromList, (!))
import           Data.Matrix
import           Data.Traversable
import qualified Data.Vector            as V
import           Errors
import           GHC.Base
import           Prelude                hiding (sequence)
import           PrettyPrint
import qualified Text.PrettyPrint.Boxes as B

data ScalarNum    = In Integer | Do Double | Ch Char
type NumMat       = Matrix ScalarNum

data Value        = I Integer | D Double | C Char | L Lambda | DF deriving Show
type MValue       = Matrix Value

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

liftBinOp :: (NumMat -> NumMat -> NumMat) -> Eval MValue -> Eval MValue -> Eval MValue
liftBinOp op v1 v2 = do {
  vv1 <- v1;
  vv2 <- v2;
  a1 <- toNumMat vv1;
  a2 <- toNumMat vv2;
   (fromNumMat (op a1 a2))
} `catchError` (\_ -> (fail _eLowLevelOperation))


instance Num ScalarNum where

  (+) (In d1) (In d2) = (In (d1 + d2))
  (+) x1 x2 = (Do (toOctaveNum(x1) + toOctaveNum(x2)))

  (*) (In d1) (In d2) = (In (d1 * d2))
  (*) x1 x2 = (Do (toOctaveNum(x1) * toOctaveNum(x2)))

  abs (In i1) = In (abs i1)
  abs x = Do (toOctaveNum(x))

  signum x = Do (signum (toOctaveNum x))

  fromInteger x = (In x)

  negate (Do x) = (Do (-1 * x))
  negate (In x) = (In (-1 * x))
  negate x = (Do (-1.0 * (toOctaveNum x)))

instance Show ScalarNum where
  show (In v) = show v
  show (Do v) = show v
  show (Ch v) = show v


toOctaveNum :: ScalarNum -> Double
toOctaveNum (In a) = fromInteger(a)
toOctaveNum (Do b) = b
toOctaveNum (Ch c) = fromIntegral(fromEnum(c))


buildBoxRow :: V.Vector ScalarNum -> B.Box
buildBoxRow r = B.hcat B.right values where
  values = V.toList (fmap convertToBox r)
  convertToBox v = B.moveRight 4 (B.text (show v))

buildBoxMatrix :: Matrix ScalarNum -> B.Box
buildBoxMatrix m = B.vcat B.top brows where
  brows = fmap buildBoxRow prows
  prows = fmap (\x -> (getRow x m)) [ 1 .. k ]
  k = (nrows m)


printRow :: V.Vector ScalarNum -> String
printRow x = L.foldl1 (GHC.Base.++) (fmap (\e -> show (x V.! e)) [ 0.. ((V.length x) - 1)])

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
  _ -> fail _eInvalidArguments

fromNum :: ScalarNum -> Eval Value
fromNum x = case x of
  (In i1) -> return (I i1)
  (Do d1) -> return (D d1)
  (Ch c1) -> return (C c1)


toNumMat :: MValue -> Eval NumMat
toNumMat v1 = sequence r
    where n = nrows v1
          m = ncols v1
          r = matrix n m (\(i,j) -> (toNum (v1 ! (i,j))))


fromNumMat :: NumMat -> Eval MValue
fromNumMat v1 = sequence r
    where n = nrows v1
          m = ncols v1
          r = matrix n m (\(i,j) -> (fromNum (v1 ! (i,j))))


single :: Value -> MValue
single x = fromList 1 1 [ x ]

getEl :: MValue -> (Int, Int) -> Eval Value
getEl m (i1, i2) = do {
  case (i1 > (nrows m), i2 > (ncols m)) of
    (False, False) -> return $ m ! (i1, i2)
    _ -> fail "Index out of bound"
}

--
