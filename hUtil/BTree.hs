module BTree where

-- Some binary tree utility stuff.

import Data.List (foldl')

data BTree a = Empty
             | Node a (BTree a) (BTree a)

instance (Show a) => Show (BTree a) where
  show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
    where
      treeshow :: Show a => String -> BTree a -> String
      treeshow pref Empty = ""
      treeshow pref (Node x Empty Empty) = (pshow pref x)
      treeshow pref (Node x l     Empty) = (pshow pref x) ++ "\n" ++
                                           (showSon pref "`--" "   " l)
      treeshow pref (Node x Empty r    ) = (pshow pref x) ++ "\n" ++
                                           (showSon pref "`--" "   " r)
      treeshow pref (Node x l     r    ) = (pshow pref x) ++ "\n" ++
                                           (showSon pref "|--" "|  " l) ++
                                           "\n" ++
                                           (showSon pref "`--" "   " r)

      showSon :: Show a => String -> String -> String -> BTree a -> String
      showSon pref before next t = pref ++ before ++ treeshow (pref ++ next) t

      pshow :: Show a => String -> a -> String
      pshow pref x = replace '\n' ("\n" ++ pref) (show x)

      replace :: Char -> String -> String -> String
      replace c new string = concatMap (change c new) string

      change :: Char -> String -> Char -> String
      change c new x | x == c    = new
                     | otherwise = [x]

instance Functor BTree where
  fmap _ Empty        = Empty
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

-- This tries to build a balanced tree that is dependent on the order of the
-- elements in the input list, where `preOrder (treeFromList l) == l`.
treeFromList :: [a] -> BTree a
treeFromList []     = Empty
treeFromList (x:xs) = Node x
                          (treeFromList (take ((length xs) `div` 2) xs))
                          (treeFromList (drop ((length xs) `div` 2) xs))

-- This builds a binary search tree.
bstreeFromList :: Ord a => [a] -> BTree a
bstreeFromList []     = Empty
bstreeFromList (x:xs) = Node x
                          (bstreeFromList (filter (<x) xs))
                          (bstreeFromList (filter (>x) xs))

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

addToTree :: Ord a => BTree a -> a -> BTree a
addToTree Empty y        = Node y Empty Empty
addToTree (Node x l r) y | y < x     = Node x (addToTree l y) r
                         | otherwise = Node x l (addToTree r y)

rmFromTree :: Ord a => BTree a -> a -> BTree a
rmFromTree Empty _        = Empty
rmFromTree (Node x l r) y |    y == x = fillUp l r
                          |    y  < x = Node x (rmFromTree l y) r
                          | otherwise = Node x l (rmFromTree r y)
  where
    fillUp :: Ord a => BTree a -> BTree a -> BTree a
    fillUp   Empty            Empty          = Empty
    fillUp   Empty            (Node y ly ry) = Node y ly ry
    fillUp   (Node x lx rx)   Empty          = Node x lx rx
    fillUp l@(Node x lx rx) r@(Node y ly ry) =
        foldl' (\rv e -> addToTree rv e) r (breadthFirst l)

treeSize :: BTree a -> Int
treeSize = length . preOrder

