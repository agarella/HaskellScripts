----
-- Non Deterministic Finite State Machine
----
module NDFSM where

import Data.List (nub)

type State = Int
type Alphabet = Char

delta :: State -> Alphabet -> [State]
delta 1 'a' = [1, 2, 3]
delta 1 'b' = [3]
delta 2 'c' = [2]
delta 2 'a' = [1]
delta _ _   = []


example = nub $ do
  x <- delta 1 'a'
  delta x 'a'

runMachine :: (State -> Alphabet -> [State]) -> State -> [Alphabet] -> [State]
runMachine d s0 string = nub $ foldr (\x y -> y >>= (\z -> d z x)) [s0] string