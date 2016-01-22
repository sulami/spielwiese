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
allPerms fn l0 l1 = concatMap (\e -> map (fn e) l1) l0

allPairs2 :: [a] -> [b] -> [(a,b)]
allPairs2 = allPerms (,)

allCards2 :: [Int] -> [String] -> [Card]
allCards2 = allPerms Card

-- 304

allPerms3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allPerms3 fn l0 l1 l2 = concatMap (\e0 ->
                          concatMap (\e1 ->
                            map (fn e0 e1) l2
                          ) l1
                        ) l0

-- 305

permStep :: [a -> b] -> [a] -> [b]
permStep fns xs = concatMap (`map` xs) fns

ap :: (a -> b -> c) -> [a] -> [b] -> [c]
ap fn = permStep . map fn

ap3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
ap3 fn l0 = permStep . ap fn l0

ap4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
ap4 fn l0 l1 = permStep . ap3 fn l0 l1

