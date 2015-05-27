{-# LANGUAGE OverloadedStrings #-}

module AST where

import           Data.Complex

data Expr =
    ConstI Integer
  | ConstD Double
  | ConstC (Complex Double)
  | Range Expr Expr
  | Str String
  | CTran Expr
  | Tran Expr
  | Matrix [ Expr ]
  | Row [ Expr ]
  | BinOp String Expr Expr
  | Unop String Expr
  | EEval String [Expr]
  | Default
  deriving Show


data Statement =
    JustExp Expr
  | Assign String (Maybe [Expr]) Expr
  | Function (Maybe String) [String] [String] [Statement]
  deriving Show
