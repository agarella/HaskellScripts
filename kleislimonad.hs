module KleisliMonad
( fmap
, (<*>)
, (>>=)
, liftM
, lift
, liftId
, runId
) where

import qualified Control.Monad as M(return, (>>=))
import qualified Control.Monad.Identity as I

import Control.Arrow

instance Monad m => Functor (Kleisli m a) where
  fmap f k = k >>= \b -> return (f b)

instance Monad m => Applicative (Kleisli m a) where
  pure g = Kleisli (\_ -> M.return g)
  f <*> g = do { b <- g; f' <- f; return (f' b) }

instance Monad m => Monad (Kleisli m a) where
  return = pure
  f >>= g = Kleisli ( \a -> (runKleisli f a) M.>>= (\b -> runKleisli (g b) a) )

type ReaderT = Kleisli

liftM :: Monad m => (a -> m b) -> Kleisli m a b
liftM f = Kleisli f

lift :: (a -> b) -> Kleisli I.Identity a b
lift f = liftM (\a -> I.Identity (f a))

liftId :: Kleisli I.Identity a a
liftId = lift id

runId :: Kleisli I.Identity a b -> a -> b
runId k a  = I.runIdentity $ runKleisli k a

-- Examples
kleisliAdd1 :: Kleisli I.Identity Int Int
kleisliAdd1 = fmap (\x -> x + 1) (lift id)

runIdKleisliAdd1 :: Int -> Int
runIdKleisliAdd1 = runId kleisliAdd1

applicativeAdd1 :: Kleisli I.Identity Int Int
applicativeAdd1 = pure (+ 1) <*> liftId

runApplicativeAdd1 :: Int -> Int
runApplicativeAdd1 = runId applicativeAdd1

-- Monadic Kleisli Composition
kleisliSquareAndDouble :: Kleisli Maybe Int Int
kleisliSquareAndDouble =
  do a <- liftM (\x -> Just (x * x))
     return (a + a)

runKleisliSquareAndDouble :: Maybe Int
runKleisliSquareAndDouble = runKleisli kleisliSquareAndDouble 5