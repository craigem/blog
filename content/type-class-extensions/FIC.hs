{-# LANGUAGE FlexibleInstances #-}

module FIC where

import Data.Set (Set, insert)
import FIA

data C = C deriving (Eq, Ord, Show)

instance Ord b => Ord (Whoopsie A b C) where
  compare (Whoopsie a1 b1 c1) (Whoopsie a2 b2 c2) =
    compare a2 a1 <> compare b1 b2 <> compare c1 c2

insC :: Ord b => Whoopsie A b C -> Set (Whoopsie A b C) -> Set (Whoopsie A b C)
insC = insert

