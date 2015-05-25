
module BuiltIns where

import           Data.Map    hiding (foldl)
import           Data.Matrix as M
import           Errors
import           Eval

evalFunction :: Lambda -> [MValue] -> Eval MValue
evalFunction (BuiltInOp1 f) [ ]        = f (single DF)
evalFunction (BuiltInOp1 f) [ v ]      = f (v)
evalFunction (BuiltInOp2 f) [ ]        = f (single DF) (single DF)
evalFunction (BuiltInOp2 f) [ v ]      = f v (single DF)
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
ones n1 m1 = (M.matrix (n1) (m1) $ \(_,_) -> (In 1))

funvect :: [(String, MValue)]
funvect = [
  ("zeros",  single (L (BuiltInOp2 (mkOp2s M.zero)))),
  ("ones",   single (L (BuiltInOp2 (mkOp2s ones)))),
  ("eye",    single (L (BuiltInOp1 (mkOp1s M.identity))))
  ]


mkOp1s :: (Int -> NumMat) -> MValue -> Eval MValue
mkOp1s f m = do {
  i1 <- getEl m (1,1);
  case i1 of
    (I i2) -> fromNumMat (f (fromInteger i2))
    _ -> fail _eInvalidArguments
}

mkOp2s :: (Int -> Int -> NumMat) -> MValue -> MValue -> Eval MValue
mkOp2s f m1 m2 = do {
  i1 <- getEl m1 (1,1);
  i2 <- getEl m2 (1,1);
  case (i1, i2) of
    (I ii1, I ii2) -> fromNumMat (f (fromInteger ii1) (fromInteger ii2))
    (I ii1, DF) -> fromNumMat    (f (fromInteger ii1) (fromInteger ii1))
    _ -> fail _eInvalidArguments
}


addBuiltIns :: Env -> Env
addBuiltIns s = foldl (\a v -> insert (fst v) (snd v) a) s funvect

initialSymTable :: Env
initialSymTable = addBuiltIns empty
