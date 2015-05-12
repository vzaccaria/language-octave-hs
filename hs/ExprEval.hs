{-# LANGUAGE OverloadedStrings #-}

module ExprEval where

import           AST
import           Control.Applicative
import           Control.Applicative.Lift
import           Data.List
import           Data.Map.Strict          (empty, lookup)
import           Data.Matrix
import           Expr
import           Symtable

eeval:: Symtable -> Expr -> Either String MOD

eeval _ (ConstI i)        = return $ fromList 1 1 [ I i ]
eeval _ (ConstD f)        = return $ fromList 1 1 [ D f ]
eeval _ (Str st)          = return $ matrix 1 (length st) $ \(_,j) -> (C (st !! (j - 1)))

eeval s (BinOp "+" e1 e2) = (liftA2 (+)) (eeval s e1) (eeval s e2)
eeval s (BinOp "-" e1 e2) = (liftA2 (-)) (eeval s e1) (eeval s e2)
eeval s (BinOp "*" e1 e2) = (liftA2 (*)) (eeval s e1) (eeval s e2)

eeval s (Row es) = Data.List.foldl1 (liftA2 (Data.Matrix.<|>)) dt
       where dt = (fmap (eeval s) es)

eeval s (Matrix es) = Data.List.foldl1 (liftA2 (Data.Matrix.<->)) dt
              where dt = (fmap (eeval s) es)

eeval s (Range a b) =
  case (f1, f2) of
    (Right (I i1), Right (I i2)) ->
      let s1 = fromInteger (i2 - i1) in
        return $ matrix 1 (s1+1) $ \(_,j) -> (I ((toInteger j) + i1 - 1))
    _ -> Left "range specification should be specified with integers"
  where
        f1 = (liftA2 (Data.Matrix.!)) (eeval s a) (Right (1,1))
        f2 = (liftA2 (Data.Matrix.!)) (eeval s b) (Right (1,1))


-- Ok, lets start with the boogie here..
eeval s (Eval var []) =
  case (Data.Map.Strict.lookup var s) of
    (Just x) -> return x
    (Nothing) -> Left ("symbol `" ++ var ++ "` not found")



-- justEvalExpression:: String -> IO String
-- justEvalExpression s = return $
--   case parseExpression s of
--     Right value -> (show (eeval empty value))
--     Left err -> show err
