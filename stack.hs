-----
-- Stack
----

import Control.Monad.State

type Stack a = [a] 

push :: a -> State (Stack) ()
push x = state $ \s -> ((), x : s)

pop :: State (Stack a) a
pop = state $ \(x : s) -> (x, s)