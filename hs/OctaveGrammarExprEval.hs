{-# LANGUAGE OverloadedStrings #-}

module OctaveGrammarExprEval where

import           Data.List
import           Data.Matrix
import           OctaveAST
import           OctaveGrammarExpr

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

first:: Matrix a -> a
first m = m ! (1,1)

eeval:: Expr -> MOD
eeval  (ConstI i)          = fromList 1 1 [ I i ]
eeval  (ConstD f)          = fromList 1 1 [ D f ]

-- Convert the string to a characters' array
eeval  (Str s) = matrix 1 (length s) $ \(_,j) -> (C (s !! (j - 1)))
eeval  (BinOp "+" e1 e2) = (eeval e1) + (eeval e2)
eeval  (BinOp "-" e1 e2) = (eeval e1) - (eeval e2)
eeval  (BinOp "*" e1 e2) = (eeval e1) * (eeval e2)
-- eeval  (BinOp "/" e1 e2) = (eeval e1) / (eeval e2)
eeval  (Row es) = Data.List.foldl1 (<|>) dt
       where dt = (fmap eeval es)

eeval  (Matrix es) = Data.List.foldl1 (<->) dt
              where dt = (fmap eeval es)

eeval  (Range a b) =
  let (I i1) = (eeval a) ! (1,1)
      (I i2) = (eeval b ) ! (1,1)
      s = fromInteger (i2 - i1) in
      matrix 1 (s+1) $ \(_,j) -> (I ((toInteger j) + i1 - 1))

justEvalExpression:: String -> IO String
justEvalExpression s = return $
  case parseExpression s of
    Right value -> "OK: " ++ (show (eeval value))
    Left err -> show err
