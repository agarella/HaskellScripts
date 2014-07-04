fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacciAux n 1 1
		where	fibonacciAux 1 n1 n2	= n1
			fibonacciAux n n1 n2	= fibonacciAux (n - 1) (n1 + n2) n1
