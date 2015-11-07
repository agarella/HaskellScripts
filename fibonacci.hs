fibonacci :: Int -> Integer
fibonacci n = last $ take (n + 1) fibonacciseq

fibonacciseq :: [Integer]
fibonacciseq = 0 : 1 : zipWith (+) fibonacciseq (tail fibonacciseq)
