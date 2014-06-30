prefixMin []	 = error "prefixMin []"
prefixMin (x:xs) = scanl min x xs

suffixMin xs = prefixMin (reverse xs)

prefixMinRec xs = calcPrefixMin xs []
suffixMinRec xs = calcPrefixMin (reverse xs) []

calcPrefixMin [] ys	= ys
calcPrefixMin (x:xs) [] = calcPrefixMin xs [x]
calcPrefixMin (x:xs) ys = calcPrefixMin xs (ys ++ [min x (last ys)])    
