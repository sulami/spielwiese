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

