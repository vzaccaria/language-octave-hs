{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

joinVec:: [String] -> String -> String
joinVec [] _ = ""
joinVec ([x]) _ = x
joinVec (x:xs) sep = x ++ sep ++ " "++ joinVec xs sep

braces:: String -> String
braces s = "(" ++ s ++ ")"

vectorAsArgs:: [String] -> String
vectorAsArgs v = joinVec v ","

asSet :: [String] -> String
asSet x = braces (joinVec x ",")
