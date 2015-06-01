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

mkOp1s :: (Int -> NumMat) -> MValue -> Eval MValue
mkOp1s f x = m2i x >>= \(In i1) -> (return $ M (f (fromInteger i1)))

mkOp2s :: (Int -> Int -> NumMat) -> MValue -> MValue -> Eval MValue
mkOp2s f x1 x2 = do {
  (In i1) <- m2i x1;
  (In i2) <- m2i x2;
  return $ M (f (fromInteger i1) (fromInteger i2))
}


addBuiltIns :: Env -> Env
addBuiltIns s = foldl (\a v -> insert (fst v) (snd v) a) s funvect

initialSymTable :: Env
initialSymTable = addBuiltIns empty
