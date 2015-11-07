module Trees where

import Prelude hiding (foldr, or, sum)
import Data.Foldable

------------------------------------------------------------------------------------------------------------------------------
-- Binary Tree
------------------------------------------------------------------------------------------------------------------------------
data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show, Eq)

root :: BTree t -> Maybe t
root Empty = Nothing
root (Node x _ _) = Just x

contains :: (Foldable t, Functor t, Eq a) => t a -> a -> Bool
contains tree x = or $ fmap (== x) tree

applicativeMax :: Ord a => Maybe a -> Maybe a -> Maybe a
applicativeMax x y = max <$> x <*> y

applicativeMin :: Ord a => Maybe a -> Maybe a -> Maybe a
applicativeMin x y = min <$> x <*> y

findMax :: Ord b => BTree b -> Maybe b
findMax t = foldr applicativeMax (root t) (fmap Just t)

findMin :: Ord b => BTree b -> Maybe b
findMin t = foldr applicativeMin (root t) (fmap Just t)

instance Functor BTree where
  fmap f Empty = Empty
  fmap f (Node x y z) = Node (f x) (fmap f y) (fmap f z)

instance Foldable BTree where
  foldr f acc Empty = acc
  foldr f acc (Node x y z) = foldr f (f x (foldr f acc z)) y