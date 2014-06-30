simplemerge xs ys = iteratesm xs ys []

iteratesm [] [] zs 		= zs
iteratesm (x:xs) [] zs		= iteratesm xs [] (zs ++ [x])
iteratesm [] (y:ys) zs 		= iteratesm [] ys (zs ++ [y])
iteratesm (x:xs) (y:ys) zs	| x <= y	= iteratesm xs (y:ys) (zs ++ [x])
				| otherwise	= iteratesm (x:xs) ys (zs ++ [y])
