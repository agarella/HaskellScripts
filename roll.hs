roll xs 0 = xs
roll [] n = []
roll (x:xs) n 	| n > 0 = roll (xs ++ [x]) (n - 1)
		| otherwise = roll (last xs : reverse (drop 1 (reverse (x:xs)))) (n + 1)
