fibonacci 0 = 1
fibonacci n = fibonacciAux n 1 1
		where	fibonacciAux 1 n1 n2	= n1
			fibonacciAux n n1 n2	= fibonacciAux (n - 1) (n1 + n2) n1

fibonacciRec 0 = 1
fibonacciRec 1 = 1
fibonacciRec n = fibonacciRec (n - 1) + fibonacciRec (n - 2)

fibonacciList n | n < 0 = error "fibonacci n < 0 undefined"
		| n == 0 = [1]
		| n == 1 = 1 : fibonacciList (n - 1)
		| otherwise = (sum.take 2) (fibonacciList (n - 1)) : fibonacciList (n - 1)
