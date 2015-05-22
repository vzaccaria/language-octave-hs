{-# LANGUAGE OverloadedStrings #-}

module ExprEval where

import           AST
import           Control.Applicative
import           Control.Applicative.Lift
import qualified Data.List                as L
import qualified Data.Map.Strict          as M (empty, lookup)
import qualified Data.Matrix              as X
import           Errors
import           Expr
import           Function
import qualified Symtable                 as S


applyNumeric2 :: (S.OctaveNumericMatrix -> S.OctaveNumericMatrix -> S.OctaveNumericMatrix) ->
                 (S.MaybeOctaveValue -> S.MaybeOctaveValue -> S.MaybeOctaveValue)
applyNumeric2 f = q where
  q (Right (S.M m1)) (Right (S.M m2)) = (Right (S.M (f m1 m2)))
  q _ _ = Left "unsupported application"


eeval:: S.Symtable -> Expr -> Either String S.OctaveValue

eeval _ (ConstI i)        = return $ S.M (X.fromList 1 1 [ S.I i ])
eeval _ (ConstD f)        = return $ S.M (X.fromList 1 1 [ S.D f ])
eeval _ (Str st)          = return $ S.M (X.matrix 1 (length st) $ \(_,j) -> (S.C (st !! (j - 1))))

eeval s (BinOp "+" e1 e2) = applyNumeric2 (+) (eeval s e1) (eeval s e2)
eeval s (BinOp "-" e1 e2) = applyNumeric2 (-) (eeval s e1) (eeval s e2)
eeval s (BinOp "*" e1 e2) = applyNumeric2 (*) (eeval s e1) (eeval s e2)

eeval s (Row es) = L.foldl1 (applyNumeric2 (X.<|>)) dt
      where dt = (fmap (eeval s) es)

eeval s (Matrix es) = L.foldl1 (applyNumeric2 (X.<->)) dt
      where dt = (fmap (eeval s) es)

eeval s (Range a b) =
  case (f1, f2) of
    (Right (S.I i1), Right (S.I i2))
      -> let s1 = fromInteger (i2 - i1) in
        return $ (S.M (X.matrix 1 (s1+1) $ \(_,j) -> (S.I ((toInteger j) + i1 - 1))))

    _
      -> _eRangeInvalid
  where
        f1 = (liftA2 (_getEl)) (eeval s a) (Right (1,1))
        f2 = (liftA2 (_getEl)) (eeval s b) (Right (1,1))
        _getEl (S.M m) x = m X.! x
        _getEl _ _ = error "Unsupported range op."


-- Ok, lets start with the boogie here..
eeval symtable (Eval var []) =
  case (M.lookup var symtable) of
    (Just x) -> return x
    (Nothing) -> Left ("symbol `" ++ var ++ "` not found")

eeval symtable (Eval symbol elist) =
  case (M.lookup symbol symtable) of
    (Just (S.F l)) -> do {
      eli <- (sequence (L.map (eeval symtable) elist));
      evalFunction l eli
    }
    Nothing -> Left "Symbol not found"
    _ -> Left "Evaluating array elements not yet implemented!"




exprValToString :: S.Symtable -> Expr -> String
exprValToString symtable expr =
    case (eeval symtable expr) of
    (Left err) -> "error: " ++ err
    (Right (S.M v)) -> (show v)
    (Right (S.F _)) -> "[Function handle]"
    (Right (S.DF)) -> "DEFAULT"
