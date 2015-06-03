{-# LANGUAGE OverloadedStrings #-}
module Errors where


_eWrongNumberOfArguments :: String
_eWrongNumberOfArguments =  ("You are passing more parameters than needed")

_eNotYetImplemented :: String
_eNotYetImplemented      =  ("Not yet implemented")

_eInvalidIndexing :: String
_eInvalidIndexing        =  ("Invalid indexes specified")

_eRangeInvalid :: String
_eRangeInvalid           =  "Range specification should be specified with integers"

_eInvalidArguments :: String
_eInvalidArguments       = "Can't interpret symbolic matrix as numeric matrix"

_symbolNotFound :: String -> String
_symbolNotFound var = ("symbol `" ++ var ++ "` not found")

_eLowLevelOperation:: String
_eLowLevelOperation=  ("Low level operation error")

_eSyntaxError::String
_eSyntaxError = "Syntax error"

_eGenericError :: String -> String
_eGenericError s = "Error: " ++ s
