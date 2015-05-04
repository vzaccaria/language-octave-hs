{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

joinVec:: [String] -> String -> String
joinVec [] _ = ""
joinVec ([x]) _ = x
joinVec (x:xs) sep = x ++ sep ++ " "++ joinVec xs sep

parens:: String -> String
parens s = "(" ++ s ++ ")"

vectorAsArgs:: [String] -> String
vectorAsArgs v = joinVec v ","

asSet :: [String] -> String
asSet x = parens (joinVec x ",")

braces:: String -> String
braces s = "{" ++ s ++ "}"
