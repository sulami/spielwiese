{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

module Set1 where

import           MCPrelude

-- 101

fiveRands :: [Integer]
fiveRands = take 5 . drop 1 . map fst $ iterate (rand . snd) (0, mkSeed 1)

-- 102

randLetter :: Seed -> (Char, Seed)
randLetter s = let (rv,ns) = rand s
                in (toLetter rv, ns)

randString3 :: String
randString3 = take 3 . drop 1 . map fst $
                iterate (randLetter . snd) (' ', mkSeed 1)


