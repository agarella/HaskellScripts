-----
-- Stack
----

import Control.Monad.State

type Stack a = [a] 

push :: a -> State (Stack a) ()
push x = state $ \s -> ((), x : s)

pop :: State (Stack a) a
pop = state $ \(x : s) -> (x, s)

-----
-- Example
----

example = runState pushValues []
  where pushValues = do push 1
                        push 2
                        push 3