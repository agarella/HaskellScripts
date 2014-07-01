data Peano = Zero | Succ Peano 
		deriving (Show, Ord, Eq)

add Zero y = y
add x Zero = x
add x (Succ y) = add (Succ x) y

peanoToNum Zero = 0
peanoToNum (Succ x) = peanoToNum x + 1

numToPeano 0 = Zero
numToPeano n = Succ (numToPeano (n - 1))

multiply x Zero 	= Zero
multiply Zero y 	= Zero
multiply (Succ Zero) y	= Succ Zero
multiply x (Succ Zero)	= Succ Zero
multiply x y 		= calcMultiply x y Zero

calcMultiply x Zero z 		= z
calcMultiply x (Succ y) z	= calcMultiply x y (add x z) 
