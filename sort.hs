module Sort
( quicksort
, mergesort
, insertionsort
, bubblesort
) where

import Data.List(delete)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = quicksort smaller ++ [x] ++ quicksort larger
  where smaller = [ s | s <- xs, s <= x]
        larger  = [ l | l <- xs, l > x]

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
  where (left, right) = splitAt (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

insertionsort :: Ord a => [a] -> [a]
insertionsort [] = []
insertionsort xs = let m = minimum xs in m : insertionsort (delete m xs)

bsort :: Ord a => [a] -> [a]
bsort [] = []
bsort [x] = [x]
bsort l @ (x : y : t)
  | isSorted l = l
  | otherwise  = if x <= y then x : bsort (y : t) else y : bsort (x : t)

isSorted :: Ord a => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs $ tail xs

doWhile :: (a -> Bool) -> (a -> a) -> a -> a
doWhile p f x = let res = f x in if not (p res) then res else doWhile p f res

bubblesort :: Ord a => [a] -> [a]
bubblesort = doWhile notSorted bsort

notSorted :: Ord a => [a] -> Bool
notSorted = (not . isSorted)