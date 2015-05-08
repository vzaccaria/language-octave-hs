{-# LANGUAGE OverloadedStrings #-}

module OctaveASTPPr where

import OctaveAST
import PrettyPrint


prExpr ::Expr -> String
prExpr  (Range (Range a b) c) =
  "range(" ++ prExpr  a ++ ", " ++ prExpr  b ++ ", "++ prExpr  c ++ ")"

prExpr  (Range a b) =
  "range(" ++ prExpr  a ++ ", " ++ prExpr  b ++ ")"

prExpr  (Default)         = "__oct__default__"
prExpr  (ConstI i)        = show i
prExpr  (ConstD f)        = show f
prExpr  (Str s)           = show s
prExpr  (Unop   s e)      = "uop"   ++ parens (vectorAsArgs [show s, (prExpr e)])
prExpr  (BinOp  s e1 e2)  = "bop"   ++ parens (vectorAsArgs [show s, (prExpr e1), (prExpr e2)])
prExpr  (Eval x e)        = "meval" ++ parens (vectorAsArgs ("ce":show x:map prExpr e))

prExpr _ = error "Sorry, need some more pattern matching here"

ps:: Statement -> String
-- ps (Eval x e) =
--
--
-- ps (Assign lval Nothing rval) =
--     "assign" ++ parens (vectorAsArgs args) where
--       args = [ show lval, pe val]
--
-- ps (Assign lval (Just l) rval) =
--     "assign" ++ parens (vectorAsArgs args) where
--             args = (show lval):(map pe l) ++ [pe rval]
--
-- ps (Function (Just name) inpar outpar slist) =
--   printf "function " ++ name ++ pars ++ (braces body)
--   where
--     pars = asSet inpar
--     body = (joinVec (fmap ps slist) ";\n")

ps _ = error "Sorry! the expression is not yet supported"
