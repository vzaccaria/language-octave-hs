
module OctaveSymtable where

import           Data.Map.Strict
import           Data.Matrix

data OD = I Integer | D Double | C Char deriving Show
type MOD = Matrix OD

instance Num OD where

  (+) (I d1) (I d2) = (I (d1 + d2))
  (+) x1 x2 = (D (toOctaveNum(x1) + toOctaveNum(x2)))

  (*) (I d1) (I d2) = (I (d1 * d2))
  (*) x1 x2 = (D (toOctaveNum(x1) * toOctaveNum(x2)))

  abs (I i1) = I (abs i1)
  abs x = D (toOctaveNum(x))

  signum x = D (signum (toOctaveNum x))

  fromInteger x = (I x)

  negate (D x) = (D (-1 * x))
  negate (I x) = (I (-1 * x))
  negate x = (D (-1.0 * (toOctaveNum x)))


toOctaveNum :: OD -> Double
toOctaveNum (I a) = fromInteger(a)
toOctaveNum (D b) = b
toOctaveNum (C c) = fromIntegral(fromEnum(c))

toNumeric:: Matrix OD -> Matrix Double
toNumeric o = fmap toOctaveNum o

type Symtable = Map String MOD

assign :: (String, MOD) -> Symtable -> Symtable
assign (k, v) s = insert k v s
