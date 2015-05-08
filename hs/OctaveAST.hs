{-# LANGUAGE OverloadedStrings #-}

module OctaveAST where

data Expr =
    ConstI Integer
  | ConstD Double
  | Range Expr Expr
  | Str String
  | Tran Expr
  | Matrix [ Expr ]
  | Row [ Expr ]
  | BinOp String Expr Expr
  | Unop String Expr
  | Eval String [Expr]
  | Default
  deriving Show


data Statement =
    Assign String (Maybe [Expr]) Expr
  | Function (Maybe String) [String] [String] [Statement]
  deriving Show
