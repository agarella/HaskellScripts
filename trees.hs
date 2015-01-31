import Prelude hiding (foldr, or)
import Data.Foldable

------------------------------------------------------------------------------------------------------------------------------
-- Binary Tree
------------------------------------------------------------------------------------------------------------------------------

data BTree a = Leaf a | Node a (BTree a) (BTree a) deriving (Show)  

contains :: (Foldable t, Functor t, Eq a) => t a -> a -> Bool
contains tree x = or $ fmap (== x) tree

instance Functor BTree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node x y z) = Node (f x) (fmap f y) (fmap f z)

instance Foldable BTree where
  foldr f acc (Leaf x) = f x acc
  foldr f acc (Node x y z) = foldr f (f x (foldr f acc z)) y