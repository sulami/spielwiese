-- god damn binary search trees

import Data.List

data BTree a = Empty | Node a (BTree a) (BTree a)
                deriving (Show)

treeFromList :: (Ord a) => [a] -> BTree a
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList $ filter (<x) xs)
                             (treeFromList $ filter (>x) xs)

main = print $ treeFromList [7, 2, 4, 8]

