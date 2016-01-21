{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

module Set3 where

import           MCPrelude

-- 301

allPairs :: [a] -> [b] -> [(a,b)]
allPairs l0 l1 = concatMap (\e -> zip (repeat e) l1) l0

-- 302

data Card = Card Int String

instance Show Card where
  show (Card x y) = show x ++ y

allCards :: [Int] -> [String] -> [Card]
allCards rs ss = concatMap (\r -> map (Card r) ss) rs

-- 303

allPerms :: (a -> b -> c) -> [a] -> [b] -> [c]
allPerms _  [] _  = []
allPerms _  _  [] = []
allPerms fn l0 l1 = concatMap (\e -> map (fn e) l1) l0

allPairs2 :: [a] -> [b] -> [(a,b)]
allPairs2 = allPerms (,)

allCards2 :: [Int] -> [String] -> [Card]
allCards2 = allPerms Card

-- 304

allPerms3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allPerms3 _  [] _  _  = []
allPerms3 _  _  [] _  = []
allPerms3 _  _  _  [] = []
allPerms3 fn l0 l1 l2 = concatMap (\e0 ->
                          concatMap (\e1 ->
                            map (fn e0 e1) l2
                          ) l1
                        ) l0

