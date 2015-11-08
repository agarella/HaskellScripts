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
  where pushValues = do _ <- push 1
                        _ <- push 2
                        z <- push 3
                        return z