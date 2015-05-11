{-# LANGUAGE OverloadedStrings #-}

module ExprEval where

import           AST
import           Data.List
import           Data.Map.Strict (empty, lookup)
import           Data.Matrix
import           Expr
import           Symtable

eeval :: Symtable -> Expr -> MOD

eeval _ (ConstI i)          = fromList 1 1 [ I i ]

eeval _ (ConstD f)          = fromList 1 1 [ D f ]

-- Convert the string to a characters' array
eeval _ (Str st) = matrix 1 (length st) $ \(_,j) -> (C (st !! (j - 1)))

eeval s (BinOp "+" e1 e2) = (eeval s e1) + (eeval s e2)

eeval s (BinOp "-" e1 e2) = (eeval s e1) - (eeval s e2)

eeval s (BinOp "*" e1 e2) = (eeval s e1) * (eeval s e2)

eeval s (Row es) = Data.List.foldl1 (<|>) dt
       where dt = (fmap (eeval s) es)

eeval s (Matrix es) = Data.List.foldl1 (<->) dt
              where dt = (fmap (eeval s) es)

eeval s (Range a b) =
  let (I i1) = (eeval s a) ! (1,1)
      (I i2) = (eeval s b ) ! (1,1)
      s1 = fromInteger (i2 - i1) in
      matrix 1 (s1+1) $ \(_,j) -> (I ((toInteger j) + i1 - 1))


-- Ok, lets start with the boogie here..
eeval s (Eval sym []) =
  case (Data.Map.Strict.lookup sym s) of
      Nothing -> error ("Sorry, symbol `"++ sym ++ "` not found")
      Just v -> v

justEvalExpression:: String -> IO String
justEvalExpression s = return $
  case parseExpression s of
    Right value -> (show (eeval empty value))
    Left err -> show err
