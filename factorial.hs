factorial :: (Num a, Eq a) => a -> a
factorial 0 = 1
factorial n = factorialAux n 1
 where factorialAux 0 acc = acc
       factorialAux n acc = factorialAux (n - 1) (n * acc)

factorialRec :: (Num a, Eq a) => a -> a
factorialRec 0 = 1
factorialRec n = factorialRec (n - 1) * n
