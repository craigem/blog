module FIA where

data A = A1 | A2 deriving (Eq, Ord, Show)

data Whoopsie a b c =
  Whoopsie a b c
  deriving (Eq, Show)
