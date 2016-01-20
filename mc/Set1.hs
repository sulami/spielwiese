{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

module Set1 where

import           MCPrelude

-- 101

fiveRands :: [Integer]
fiveRands = take 5 . drop 1 . map fst $ iterate (rand . snd) (0, mkSeed 1)


