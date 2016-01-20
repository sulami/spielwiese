{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

module Set3 where

import           MCPrelude

-- 301

allPairs :: [a] -> [b] -> [(a,b)]
allPairs l0 l1 = concatMap (\e -> zip (repeat e) l1) l0

