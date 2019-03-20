{-# LANGUAGE FlexibleInstances #-}

module FIB where

import Data.Set (Set, insert)
import FIA

data B = B deriving (Eq, Ord, Show)

instance Ord c => Ord (Whoopsie A B c) where
  compare (Whoopsie a1 b1 c1) (Whoopsie a2 b2 c2) =
    compare a1 a2 <> compare b1 b2 <> compare c1 c2

insB :: Ord c => Whoopsie A B c -> Set (Whoopsie A B c) -> Set (Whoopsie A B c)
insB = insert
