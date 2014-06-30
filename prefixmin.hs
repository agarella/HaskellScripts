prefixmin xs = calcprefixmin xs []

suffixmin xs = calcprefixmin (reverse xs) []

calcprefixmin [] ys  = ys
calcprefixmin (x:xs) [] = calcprefixmin xs [x]
calcprefixmin (x:xs) ys = calcprefixmin xs (ys ++ [min x (last ys)])    
