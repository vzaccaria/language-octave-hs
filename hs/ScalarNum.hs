{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ScalarNum where

import           Data.Complex
import           Data.Ratio

data ScalarNum = In Integer | Do Double | Ch Char | Co (Complex Double)


sconjugate :: (ScalarNum) -> (ScalarNum)
sconjugate c = lift1 conjugate c

lift1 :: ((Complex Double) -> (Complex Double)) -> ScalarNum -> ScalarNum
lift1 f x1 = Co (f (toOctaveNum x1))

lift2 :: ((Complex Double) -> (Complex Double) -> (Complex Double)) -> ScalarNum -> ScalarNum -> ScalarNum
lift2 f x1 x2 = Co (f (toOctaveNum x1) (toOctaveNum x2))


instance Num (ScalarNum) where

  (+) (In d1) (In d2) = (In (d1 + d2))
  (+) x1 x2 = lift2 (+) x1 x2

  (*) (In d1) (In d2) = (In (d1 * d2))
  (*) x1 x2 = lift2 (*) x1 x2

  abs (In i1) = In (abs i1)
  abs (Do d1) = Do (abs (d1))
  abs (Co c1) = Co (abs (c1))
  abs (Ch c1) = Ch c1

  signum x = lift1 signum x

  fromInteger x = (In x)

  negate (Do x) = (Do (-1 * x))
  negate (In x) = (In (-1 * x))
  negate x = lift1 ((-1.0) *) x

instance Show (ScalarNum) where
  show (In v) = show v
  show (Do v) = show v
  show (Ch v) = show v
  show (Co v) = show (realPart v) ++ " + " ++ show (imagPart v) ++ "i"

instance Fractional (ScalarNum) where
  fromRational (r) = let n = In (numerator r)
                         d = In (denominator r) in
                         n/d
  recip (In d1) = Do (1.0/(fromInteger d1))
  recip (Do d1) = Do (1.0/d1)
  recip (Co d1) = Co (1.0/d1)
  recip d1 = Co (1.0/(toOctaveNum d1))



toOctaveNum :: (ScalarNum) -> (Complex Double)
toOctaveNum (In a) = fromInteger(a)
toOctaveNum (Do b) = (b :+ 0)
toOctaveNum (Ch c) = fromIntegral(fromEnum(c))
toOctaveNum (Co c) = c
