prefixMin xs = calcPrefixMin xs []

suffixMin xs = calcPrefixMin (reverse xs) []

calcPrefixMin [] ys	= ys
calcPrefixMin (x:xs) [] = calcPrefixMin xs [x]
calcPrefixMin (x:xs) ys = calcPrefixMin xs (ys ++ [min x (last ys)])    
