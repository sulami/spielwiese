{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.List (sortBy)
import Data.Maybe (fromJust)
import System.Random (randomRIO)

type Property   = (String, IO Float)
type Ent        = [Property]
type Generation = [Ent]

data Config     = Config {
    genSize         :: Int,
    mutProbability  :: Float,
    mutRange        :: Float
  }

mutate :: Config -> Ent -> [Ent]
mutate conf base = (base : mutate conf (mut base))
  where
    mut :: Ent -> Ent
    mut base = map chProp base

    chProp :: Property -> Property
    chProp (n, p) = let np = do r1 <- randomRIO (0, 1) :: IO Float
                                let v = if r1 <= mutProbability conf
                                        then randomRIO (0, mutRange conf)
                                        else return 0
                                nv <- v
                                op <- p
                                return (op * (1 + nv))
                    in (n, np)

evolution :: Config -> Ent -> Generation
evolution conf base = take (genSize conf) $ mutate conf base

best :: Ord a => (Ent -> a) -> Generation -> Ent
best f gen = fst $ last $ sortBy s $ zip gen $ map f gen

run :: Ord a => Config -> Generation -> (Ent -> a) -> [Ent]
run conf gen f = let winner = best f gen
                     ngen = evolution conf winner
                  in winner : run conf ngen f

s :: Ord a => (b, a) -> (b, a) -> Ordering
s (_, a) (_, b) | a > b     = GT
                | a < b     = LT
                | otherwise = EQ

printGen :: Generation -> IO ()
printGen []     = return ()
printGen (x:xs) = do printEnt x
                     printGen xs

printEnt :: Ent -> IO ()
printEnt []     = return ()
printEnt (x:xs) = do putStr $ fst x ++ ": "
                     v <- snd x
                     print v
                     printEnt xs

main = do let conf = Config 5 0.5 0.1
          let base = [("Height", return 100), ("Weight", return 80)] :: Ent
          let test = (\e -> 1)
          let gen0 = evolution conf base
          let evo = take 5 $ run conf gen0 test
          printGen evo

