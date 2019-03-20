module Main where

import Data.Set (Set, empty)

import FIA
import FIB
import FIC

test :: Set (Whoopsie A B C)
test =
  insB (Whoopsie A1 B C) . insC (Whoopsie A1 B C) . insC (Whoopsie A2 B C) $ empty

main :: IO ()
main =
  print test
