{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ScalarNumMat where

import           Data.List              as L
import           Data.Matrix
import qualified Data.Vector            as V
import           GHC.Base
import           ScalarNum
import qualified Text.PrettyPrint.Boxes as B

type NumMat       = Matrix ScalarNum

conj :: NumMat -> NumMat
conj m = fmap (sconjugate) m


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
