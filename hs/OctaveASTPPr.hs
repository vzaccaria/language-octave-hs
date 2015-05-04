{-# LANGUAGE OverloadedStrings #-}

module OctaveASTPPr where

import PrettyPrint
import Debug.Trace
import Text.Printf
import OctaveAST


pe::Expr -> String

pe (Cons (Cons a b) c) =
  "range(" ++ pe a ++ "," ++ pe b ++ ", "++ pe c ++ ")"

pe (Cons a b) =
  "range(" ++ pe a ++ "," ++ pe b ++ ")"

pe (Default) = "__oct__default__"
pe (ConstI i) = show i

ps:: Statement -> String

ps (Eval x e) =
  "meval" ++ parens (vectorAsArgs (show x:map pe e))

ps (Assign lval Nothing rval) =
    "assign" ++ parens (vectorAsArgs args) where
      args = [ show lval, pe rval]

ps (Assign lval (Just l) rval) =
    "assign" ++ parens (vectorAsArgs args) where
            args = (show lval):(map pe l) ++ [pe rval]

ps (Function (Just name) inpar outpar slist) =
  printf "function " ++ name ++ pars ++ (braces body)
  where
    pars = asSet inpar
    body = (joinVec (fmap ps slist) ";\n")

ps _ = error "Sorry! the expression is not yet supported"


testFun (e,s) = (ps e) == (trace ("actual: "++(ps e) ++ " expected: "++s) s)

test tts = and (map testFun tts)

exampleStatement = (Eval "pluto" [Default, Default])
exampleFunction = (Function (Just "pippo") ["a1","a2"] ["a3","a4"] [ exampleStatement ])

mytests = [
  ((Eval "pluto" [Cons (ConstI 1) (ConstI 2)]),                      "meval(\"pluto\", range(1,2))"),
  ((Assign "pluto" (Just [Cons (ConstI 1) (ConstI 2)]) (ConstI 1)),  "assign(\"pluto\", range(1,2), 1)"),
  (exampleStatement,                                "meval(\"pluto\", __oct__default__, __oct__default__)"),
  (exampleFunction, "function pippo(a1, a2){meval(\"pluto\", __oct__default__, __oct__default__)}")
  ]

testThisModule :: Bool
testThisModule = test mytests
