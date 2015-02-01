import Prelude hiding (foldr, or)
import Data.Foldable
import Data.Monoid

------------------------------------------------------------------------------------------------------------------------------
-- Binary Tree
------------------------------------------------------------------------------------------------------------------------------
data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show, Eq)

root :: BTree t -> t
root Empty = error "root Empty"
root (Node x _ _) = x

contains :: (Foldable t, Functor t, Eq a) => t a -> a -> Bool
contains tree x = or $ fmap (== x) tree

findMax :: Ord b => BTree b -> b
findMax t = foldr max (root t) t
findMin :: Ord b => BTree b -> b
findMin t = foldr min (root t) t

instance Functor BTree where
  fmap f Empty = Empty
  fmap f (Node x y z) = Node (f x) (fmap f y) (fmap f z)

instance Foldable BTree where
  foldr f acc Empty = acc
  foldr f acc (Node x y z) = foldr f (f x (foldr f acc z)) y