{-# LANGUAGE OverloadedStrings #-}

module OctaveAST where

data Expr =
    ConstI Integer
  | ConstF Float
  | Vector (Range Expr)
  | Default
  | Eval String [Expr]
  | Assign String (Maybe [Expr]) Expr
  deriving Show

data Range a =
      MinMaxStep a a a
    | MinMax a a
    deriving Show

joinVec:: [String] -> String -> String
joinVec [] _ = ""
joinVec ([x]) _ = x
joinVec (x:xs) sep = x ++ sep ++ joinVec xs sep

braces:: String -> String
braces s = "(" ++ s ++ ")"

vectorAsArgs:: [String] -> String
vectorAsArgs v = joinVec v ","

ppr::Expr -> String

ppr (Eval x e) =
  "meval" ++ braces (vectorAsArgs (show x:map ppr e))

ppr (Vector (MinMax a b)) =
  "range(" ++ ppr a ++ "," ++ ppr b ++ ")"

ppr (Vector (MinMaxStep a b s)) =
    "range(" ++ ppr a ++ "," ++ ppr b ++ ", "++ ppr s ++ ")"

ppr (Default) = "__oct__default__"
ppr (ConstI i) = show i

ppr (Assign lval Nothing rval) =
    "assign" ++ braces (vectorAsArgs args) where
      args = [ show lval, ppr rval]

ppr (Assign lval (Just l) rval) =
    "assign" ++ braces (vectorAsArgs args) where
            args = (show lval):(map ppr l) ++ [ppr rval]

ppr _ = error "Sorry! the expression is not yet supported"

main :: IO ()
main = do
  putStrLn $ ppr (Eval "pluto" [Vector (MinMax (ConstI 1) (ConstI 2))])
  putStrLn $ ppr (Eval "pluto" [Default, Default])
  putStrLn $ ppr (Assign "pluto" (Just [Vector (MinMax (ConstI 1) (ConstI 2))]) (ConstI 1))
