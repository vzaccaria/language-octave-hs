
module Function where


import qualified Data.Matrix as M
import qualified Symtable    as S


evalFunction :: S.Lambda -> [S.MOD] -> S.MOD
evalFunction (S.Op1 f) [ ] = f (S.DF)
evalFunction (S.Op1 f) [ v ] = f (v)
evalFunction (S.Op2 f) [ ] = f S.DF S.DF
evalFunction (S.Op2 f) [ v ] = f v S.DF
evalFunction (S.Op2 f) [ v , v2 ] = f v v2
evalFunction (S.Op1 _) _ = error "You are passing more parameters than needed"
evalFunction (S.Op2 _) _ = error "You are passing more parameters than needed"
evalFunction _ _ = error "Lambda not yet implemented"

--  ____        _ _ _   _
-- | __ ) _   _(_) | |_(_)_ __  ___
-- |  _ \| | | | | | __| | '_ \/ __|
-- | |_) | |_| | | | |_| | | | \__ \
-- |____/ \__,_|_|_|\__|_|_| |_|___/


zeros :: S.MOD -> S.MOD -> S.MOD
zeros (S.M m1) (S.M m2)  =
  case (f1, f2) of
    (S.I i1, S.I i2) -> S.M (M.zero (fromInteger i1) (fromInteger i2))
    _ -> error "not supported zero creation"
    where
      f1 = m1 M.! (1,1)
      f2 = m2 M.! (1,1)
zeros _ _ = error "You should invoke zeros with the correct number of items!"


addBuiltIns :: S.Symtable -> S.Symtable
addBuiltIns s = S.assign ("zeros", (S.F (S.Op2 zeros))) s

initialSymTable :: S.Symtable
initialSymTable = addBuiltIns S.emptyTable
