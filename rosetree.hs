------------------------------------------------------------------------------------------------------------------------------
-- Rose Tree
------------------------------------------------------------------------------------------------------------------------------

data Rose a = a :> [Rose a] deriving Show

root :: Rose a -> a
root (x :> _) = x

children :: Rose a -> [Rose a]
children (_:> xs) = xs

instance Functor Rose where
  fmap f (x :> xs) = f x :> map (fmap f) xs

instance Foldable Rose where
  foldMap f (x :> xs) = (f x) `mappend` (foldr (\y acc -> foldMap f y `mappend` acc) mempty xs)