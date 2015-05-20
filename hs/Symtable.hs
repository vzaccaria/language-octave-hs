{-# LANGUAGE OverloadedStrings #-}

module Symtable where

import           AST
import qualified Data.List              as L
import           Data.Map.Strict
import qualified Data.Matrix            as M
import qualified Data.Vector            as V
import           GHC.Base
import qualified Text.PrettyPrint.Boxes as B

--  __  __       _        _
-- |  \/  | __ _| |_ _ __(_)_  __
-- | |\/| |/ _` | __| '__| \ \/ /
-- | |  | | (_| | |_| |  | |>  <
-- |_|  |_|\__,_|\__|_|  |_/_/\_\
--

--                   invar   outvar   body
data Lambda =
  Lam [String] [String] [Statement] |
  Op1 (MOD -> MOD) |
  Op2 (MOD -> MOD -> MOD)


data OD = I Integer | D Double | C Char
data MOD = M (M.Matrix OD) | F Lambda | DF

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

instance Show OD where
  show (I v) = show v
  show (D v) = show v
  show (C v) = show v


toOctaveNum :: OD -> Double
toOctaveNum (I a) = fromInteger(a)
toOctaveNum (D b) = b
toOctaveNum (C c) = fromIntegral(fromEnum(c))


buildBoxRow :: V.Vector OD -> B.Box
buildBoxRow r = B.hcat B.right values where
  values = V.toList (fmap convertToBox r)
  convertToBox v = B.moveRight 4 (B.text (show v))

buildBoxMatrix :: M.Matrix OD -> B.Box
buildBoxMatrix m = B.vcat B.top brows where
  brows = fmap buildBoxRow prows
  prows = fmap (\x -> (M.getRow x m)) [ 1 .. k ]
  k = (M.nrows m)


printRow :: V.Vector OD -> String
printRow x = L.foldl1 (GHC.Base.++) (fmap (\e -> show (x V.! e)) [ 0.. ((V.length x) - 1)])

printMOD :: MOD -> String
printMOD (M m) = B.render (buildBoxMatrix m)
printMOD (DF)  = ":"
printMOD (F _) = error "Lambdas are not printable at the moment"

-- printMOD v = L.foldl1 (GHC.Base.++) (fmap (\x -> printRow (getRow x v)) [ 1 .. k ])
          -- where k = (nrows v)

--  ____                  _        _     _
-- / ___| _   _ _ __ ___ | |_ __ _| |__ | | ___
-- \___ \| | | | '_ ` _ \| __/ _` | '_ \| |/ _ \
--  ___) | |_| | | | | | | || (_| | |_) | |  __/
-- |____/ \__, |_| |_| |_|\__\__,_|_.__/|_|\___|
--        |___/
--


type Symtable = Map String MOD

assign :: (String, MOD) -> Symtable -> Symtable
assign (k, v) s = insert k v s

emptyTable :: Map k a
emptyTable = empty
