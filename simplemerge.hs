simpleMerge xs ys = recSimpleMerge xs ys []

recSimpleMerge [] [] zs		= zs
recSimpleMerge (x:xs) [] zs	= recSimpleMerge xs [] (zs ++ [x])
recSimpleMerge [] (y:ys) zs 	= recSimpleMerge [] ys (zs ++ [y])
recSimpleMerge (x:xs) (y:ys) zs	| x <= y	= recSimpleMerge xs (y:ys) (zs ++ [x])
				| otherwise	= recSimpleMerge (x:xs) ys (zs ++ [y])
