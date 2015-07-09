module BTree where

-- Some binary tree utility stuff.

data BTree a = Empty
             | Node a (BTree a) (BTree a)
  deriving Show

instance Functor BTree where
  fmap _ Empty        = Empty
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

treeFromList :: Ord a => [a] -> BTree a
treeFromList []     = Empty
treeFromList (x:xs) = Node x
                          (treeFromList (filter (<x) xs))
                          (treeFromList (filter (>x) xs))

preOrder :: BTree a -> [a]
preOrder Empty        = []
preOrder (Node x l r) = x : (preOrder l) ++ (preOrder r)

inOrder :: BTree a -> [a]
inOrder Empty        = []
inOrder (Node x l r) = (inOrder l) ++ [x] ++ (inOrder r)

postOrder :: BTree a -> [a]
postOrder Empty        = []
postOrder (Node x l r) = (postOrder l) ++ (postOrder r) ++ [x]

breadthFirst :: BTree a -> [a]
breadthFirst t = bf [t]
  where
    bf :: [BTree a] -> [a]
    bf [] = []
    bf t  = map nodeValue t ++ bf (concat (map leftAndRight t))
    nodeValue :: BTree a -> a
    nodeValue (Node x _ _) = x
    leftAndRight :: BTree a -> [BTree a]
    leftAndRight (Node _ Empty Empty) = []
    leftAndRight (Node _ Empty r    ) = [r]
    leftAndRight (Node _ l     Empty) = [l]
    leftAndRight (Node _ l     r    ) = [l, r]

