quickSort [] = []
quickSort (x:xs) = quickSort ys ++ [x] ++ quickSort zs
	where
		ys = [a | a <- xs, a <= x]
		zs = [b | b <- xs, b > x]
