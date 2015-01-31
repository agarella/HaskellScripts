------------------------------------------------------------------------------------------------------------------------------
-- Binary Tree
------------------------------------------------------------------------------------------------------------------------------

data BTree a = Leaf a | Node a (BTree a) (BTree a) deriving (Show)  

root (Leaf x) = x
root (Node x y z) = x

instance Functor BTree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node x y z) = Node (f x) (fmap f y) (fmap f z)