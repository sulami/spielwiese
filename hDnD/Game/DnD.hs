-- |
-- Module      :  Game.DnD
-- Copyright   :  Robin Schroer 2015
-- License     :  BSD3
--
-- Maintainer  :  sulami@peerwire.org
-- Stability   :  experimental
-- Portability :  unknown
--
-- A Dungeons and Dragons™ utiltiy library. Probably also useful for other
-- RPGs.
--

module Game.DnD where

import           Control.Monad.State -- (State, get, put)
import           System.Random (StdGen, getStdGen, mkStdGen, randomR, randomRs)

-- | Use an initial integer to deterministically initialize the RNG.
initialize :: Int -> StdGen
initialize = mkStdGen

-- | Use the system-wide RNG to randomly initialize the RNG.
initializeIO :: IO StdGen
initializeIO = getStdGen

-- | Use the system-wide RNG to use any of the pure functions.
rollIO :: (StdGen -> a) -> IO a
rollIO f = getStdGen >>= return . f

-- | Use the system-wide RNG to use any of the pure functions with a parameter.
roll1IO :: (StdGen -> b -> a) -> b -> IO a
roll1IO f p0 = do g <- getStdGen
                  return $ f g p0

-- | Use the system-wide RNG to use any of the pure functions with two
-- parameters.
roll2IO :: (StdGen -> c -> b -> a) -> c -> b -> IO a
roll2IO f p0 p1 = do g <- getStdGen
                     return $ f g p0 p1

-- | Roll a single die with d sides.
roll :: StdGen -> Int -> Int
roll g d = head $ rolls g 1 d

-- | Roll n dice with d sides each and return all the results in a list.
rolls :: StdGen -> Int -> Int -> [Int]
rolls g n d = take n $ randomRs (1, d) g

-- | Get a single random element from a list.
randFromList :: StdGen -> [a] -> Maybe a
randFromList _ [] = Nothing
randFromList g l  = Just $ head $ randsFromList g 1 l

-- | Get a number of random, unique elements from a list. Can return at most
-- all elements from the list once.
randsFromList :: StdGen -> Int -> [a] -> [a]
randsFromList _ _ [] = []
randsFromList _ 0 _  = []
randsFromList g n l  = let (i, g') = randomR (0, length l - 1) g
                           l' = take i l ++ drop (i+1) l
                       in l !! i : randsFromList g' (n-1) l'

-- | Determine the chance that something happens.
chance :: StdGen -> Int -> Bool
chance g c = c <= roll g 100

-- | Flip a coin.
flipCoin :: StdGen -> Bool
flipCoin g = chance g 50

-- | Roll a D100.
d100 :: StdGen -> Int
d100 g = roll g 100

-- | Roll a D20.
d20 :: StdGen -> Int
d20 g = roll g 20

-- | Roll a D12.
d12 :: StdGen -> Int
d12 g = roll g 12

-- | Roll a D10.
d10 :: StdGen -> Int
d10 g = roll g 10

-- | Roll a D8.
d8 :: StdGen -> Int
d8 g = roll g 8

-- | Roll a D6.
d6 :: StdGen -> Int
d6 g = roll g 6

-- | Roll a D4.
d4 :: StdGen -> Int
d4 g = roll g 4

