{-# LANGUAGE OverloadedStrings #-}
module Errors where


_eWrongNumberOfArguments :: Either String b
_eWrongNumberOfArguments = Left ("You are passing more parameters than needed")

_eNotYetImplemented :: Either [Char] b
_eNotYetImplemented      = Left ("Lambda not yet implemented")

_eInvalidIndexing :: Either String b
_eInvalidIndexing        = Left ("Invalid indexes specified")

_eRangeInvalid :: Either String b
_eRangeInvalid           = Left "range specification should be specified with integers"
