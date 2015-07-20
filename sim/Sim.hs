-- | This is a simple, generic evolution library. It can use arbitrary entities
-- and scoring functions to find optimal solutions for a given set of numerical
-- properties by trial and error and natural selection.
--
-- Example usage looks like this:
--
-- > module Main where
-- >
-- > import Sim
-- >
-- > main = do let conf = Config 1000 0.2 0.1
-- >           let base = [("Height", return 200), ("Weight", return 80)]
-- >           let test = (\ent -> 1) -- No actual ordering going on here.
-- >           let gen0 = evolution conf base
-- >           printGen $ take 5 $ run conf gen0 test

module Sim (
  Property, Ent, Generation, Config (..),
  evolution, run, printEnt, printGen
) where

import Data.List (sortBy)
import System.Random (randomRIO)

-- | A pair of a string that describes the property and an IO Float that is the
-- value of the property.
type Property   = (String, IO Float)
-- | An entity, which is just a list of properties.
type Ent        = [Property]
-- | A list of Ents.
type Generation = [Ent]

-- | A set of options that define the parameters of the evolution.
data Config     = Config {
    -- | The size of each generation.
    genSize         :: Int,
    -- | The probability for each property to mutate at all, e.g. 0.1 for 10%.
    mutProbability  :: Float,
    -- | The range in which a mutating property will be able to change, e.g.
    -- 0.2 for changes up to 20%.
    mutRange        :: Float
  }

mutate :: Config -> Ent -> [Ent]
mutate conf base = mut base : mutate conf base
  where
    mut :: Ent -> Ent
    mut base = map chProp base

    chProp :: Property -> Property
    chProp (n, p) = let np = do r1 <- randomRIO (0, 1) :: IO Float
                                let v = if r1 <= mutProbability conf
                                        then randomRIO (-1 * mutRange conf,
                                                        mutRange conf)
                                        else return 0
                                nv <- v
                                op <- p
                                return (op * (1 + nv))
                    in (n, np)

-- | Generate a generation using a config and an ent as base. Useful to
-- generate the first generation to use with 'run'.
evolution :: Config -> Ent -> Generation
evolution conf base = take (genSize conf) $ base : mutate conf base

best :: Ord a => (Ent -> a) -> Generation -> Ent
best f gen = fst $ last $ sortBy s $ zip gen $ map f gen

-- | The main loop of this library. It produces a lazy infinite list of
-- entities that are the best in their respective generation, generated using
-- the config supplied and compared using the scoring function.
--
-- > take 10 $ run conf gen0 scoring
--
-- would run the simulation for 10 generations and return a list of 10
-- entities.
--
-- The scoring function should take an ent and return a value that can be used
-- to sort the generation and choose a winner that will act as the base for the
-- next generation.
run :: Ord a => Config -> Generation -> (Ent -> a) -> [Ent]
run conf gen f = let winner = best f gen
                     ngen = evolution conf winner
                  in winner : run conf ngen f

s :: Ord a => (b, a) -> (b, a) -> Ordering
s (_, a) (_, b) | a > b     = GT
                | a < b     = LT
                | otherwise = EQ

-- | Print out an entire generation, or any other list of ents.
printGen :: Generation -> IO ()
printGen []     = return ()
printGen (x:xs) = do printEnt x
                     printGen xs

-- | Print out a single ent with all its properties.
printEnt :: Ent -> IO ()
printEnt []     = return ()
printEnt (x:xs) = do putStr $ fst x ++ ": "
                     v <- snd x
                     print v
                     printEnt xs

