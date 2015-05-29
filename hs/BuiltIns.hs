{-# LANGUAGE CPP #-}

module BuiltIns where

import           Data.Map     hiding (foldl)
import           Data.Matrix  as M
import           Errors
import           Eval
import           ScalarNum
import           ScalarNumMat

-- #define G(M, i, j)


evalFunction :: Lambda -> [MValue] -> Eval MValue
evalFunction (BuiltInOp1 f) [ ]        = f DF
evalFunction (BuiltInOp1 f) [ v ]      = f v
evalFunction (BuiltInOp2 f) [ ]        = f DF DF
evalFunction (BuiltInOp2 f) [ v ]      = f v DF
evalFunction (BuiltInOp2 f) [ v , v2 ] = f v v2
evalFunction (BuiltInOp2 _) _          = fail _eWrongNumberOfArguments
evalFunction (BuiltInOp1 _) _          = fail _eWrongNumberOfArguments
evalFunction _ _                       = fail _eNotYetImplemented

-- --  ____        _ _ _   _
-- -- | __ ) _   _(_) | |_(_)_ __  ___
-- -- |  _ \| | | | | | __| | '_ \/ __|
-- -- | |_) | |_| | | | |_| | | | \__ \
-- -- |____/ \__,_|_|_|\__|_|_| |_|___/

ones :: Int -> Int -> NumMat
ones n1 m1 = M.matrix n1 m1 $ \(_,_) -> (In 1)

funvect :: [(String, MValue)]
funvect = [
  ("zeros",  (L (BuiltInOp2 (mkOp2s M.zero)))),
  ("ones",   (L (BuiltInOp2 (mkOp2s ones)))),
  ("eye",    (L (BuiltInOp1 (mkOp1s M.identity))))
  ]

getFirstInt :: MValue -> Eval Integer
getFirstInt m = do {
  i1 <- getEl m (1,1);
  case i1 of
    (I i2) -> return i2
}

mkOp1s :: (Int -> NumMat) -> MValue -> Eval MValue
mkOp1s f (M m) = do {
  i1 <- getFirstInt (M m);
  fromNumMat(f (fromInteger i1))
}

mkOp2s :: (Int -> Int -> NumMat) -> MValue -> MValue -> Eval MValue
mkOp2s f (M m1) (M m2) = do {
  i1 <- getFirstInt (M m1);
  i2 <- getFirstInt (M m2);
  fromNumMat (f (fromInteger i1) (fromInteger i2))
}


addBuiltIns :: Env -> Env
addBuiltIns s = foldl (\a v -> insert (fst v) (snd v) a) s funvect

initialSymTable :: Env
initialSymTable = addBuiltIns empty
