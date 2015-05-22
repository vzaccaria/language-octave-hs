
module Function where


import qualified Data.Matrix as M
import           Errors
import qualified Symtable    as S

evalFunction :: S.Lambda -> [S.MOD] -> Either String S.MOD
evalFunction (S.Op1 f) [ ]        = f (S.DF)
evalFunction (S.Op1 f) [ v ]      = f (v)
evalFunction (S.Op2 f) [ ]        = f S.DF S.DF
evalFunction (S.Op2 f) [ v ]      = f v S.DF
evalFunction (S.Op2 f) [ v , v2 ] = f v v2
evalFunction (S.Op1 _) _          = _eWrongNumberOfArguments
evalFunction (S.Op2 _) _          = _eWrongNumberOfArguments
evalFunction _ _                  = _eNotYetImplemented

--  ____        _ _ _   _
-- | __ ) _   _(_) | |_(_)_ __  ___
-- |  _ \| | | | | | __| | '_ \/ __|
-- | |_) | |_| | | | |_| | | | \__ \
-- |____/ \__,_|_|_|\__|_|_| |_|___/



ones :: Int -> Int -> S.OctaveNumericMatrix
ones n1 m1 = (M.matrix n1 m1 $ \(_,_) -> (S.I 1))

funvect :: [(String, S.OctaveValue)]
funvect = [
  ("zeros",  (S.F (S.Op2 (mkOp2 M.zero)))),
  ("ones",   (S.F (S.Op2 (mkOp2 ones)))),
  ("eye",    (S.F (S.Op1 (mkOp1 M.identity))))
  ]

ofIntM :: (Int, Int) -> S.OctaveNumericMatrix -> Either String Int
ofIntM (p1, p2) m1 = case (f1) of
  (S.I i1) -> Right (fromInteger i1)
  _ -> _eInvalidIndexing
  where
    f1 = m1 M.! (p1, p2)

mkOp1 :: (Int -> (S.OctaveNumericMatrix)) -> S.OctaveValue -> Either String S.OctaveValue
mkOp1 f (S.M m1) = do {
  i1 <- (1,1) `ofIntM` m1;
  return $ S.M (f i1)
}

mkOp1 _ _ = _eWrongNumberOfArguments

mkOp2 :: (Int -> Int -> (S.OctaveNumericMatrix)) -> S.OctaveValue -> S.OctaveValue -> Either String S.OctaveValue
mkOp2 f (S.M m1) (S.M m2) = do {
  i1 <- (1,1) `ofIntM` m1;
  i2 <- (1,1) `ofIntM` m2;
  return $ S.M (f i1 i2)
}

mkOp2 f (S.M m1) (S.DF) = do {
  i1 <- (1,1) `ofIntM` m1;
  return $ S.M (f i1 i1)
  }

mkOp2 _ _ _ = _eWrongNumberOfArguments



addBuiltIns :: S.Symtable -> S.Symtable
addBuiltIns s = foldl (\a v -> S.assign v a) s funvect

initialSymTable :: S.Symtable
initialSymTable = addBuiltIns S.emptyTable
