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

-- 103

type Gen a = Seed -> (a, Seed)

generalA :: (a -> a) -> Gen a -> Gen a
generalA fn gen s = let (rv,ns) = gen s
                    in (fn rv, ns)

randEven :: Gen Integer
randEven = generalA (*2) rand

randOdd :: Gen Integer
randOdd = generalA (+1) randEven

randTen :: Gen Integer
randTen = generalA (*10) rand

-- 104

randPair :: Gen (Char, Integer)
randPair = generalB (\n -> (toLetter n, n^2)) rand

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair g0 g1 s = let (rv0,ns0) = g0 s
                          (rv1,ns1) = g1 s
                      in ((rv0,rv1), ns0)

generalB :: (a -> b) -> Gen a -> Gen b
generalB fn gen s = let (rv,ns) = gen s
                    in (fn rv, ns)


