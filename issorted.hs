isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted (x:xs) = and [x <= y | (x, y) <- pairs]
 where pairs = zip (x:xs) xs
