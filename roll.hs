roll xs 0 = xs
roll [] n = []
roll (x:xs) n = roll (xs ++ [x]) (n - 1)
