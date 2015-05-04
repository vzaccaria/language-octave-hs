{-# LANGUAGE OverloadedStrings #-}

module OctaveAST where

data Expr =
    ConstI Integer
  | ConstD Double
  | Cons Expr Expr
  | Matrix [ Expr ]
  | Row [ Expr ]
  | VectorValues Integer Integer [ Expr ]
  | BinOp String Expr Expr
  | Unop String Expr
  | Default
  deriving Show

data Statement =
    Eval String [Expr]
  | Assign String (Maybe [Expr]) Expr
  | Return
  | Function (Maybe String) [String] [String] [Statement]
  deriving Show
