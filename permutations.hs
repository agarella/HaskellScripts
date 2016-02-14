module Permutations
( permutations
) where

import Data.List(delete)

permutations :: Eq a => [a] -> [[a]]
permutations [x] = [return x]
permutations xs = 
  do x <- xs
     rest <- permutations $ delete x xs
     return (x : rest)