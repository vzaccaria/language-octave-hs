{-# LANGUAGE OverloadedStrings #-}

module Main where

import           GrammarEval

main :: IO ()
main = putStrLn $ evalProgram " a = 2 + 2 "
