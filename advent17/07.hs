#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

import           Data.List (groupBy, isInfixOf, sortBy)
import           Data.Ord  (comparing)

data Program = Program
  { name     :: String
  , weight   :: Int
  , carrying :: [Program]
  , string   :: String
  } deriving Show

type ProgramWeight = (Int, Int)

readProgram :: String -> Program
readProgram s = let (n:ws:_) = words s
                    w = read $ filter (`elem` ['0'..'9']) ws
                in Program n w [] s

main :: IO ()
main = do
  input <- lines <$> getContents
  let root = findRoot input
  putStrLn $ name root
  let tree = buildTree root input
      imbalanced = last . filter (not. balanced) $ trav tree
      imbalancedWeights = map weights $ carrying imbalanced
  print $ balance imbalancedWeights

findRoot :: [String] -> Program
findRoot []     = undefined
findRoot (x:xs) = findRoot' (readProgram x)
  where
    findRoot' :: Program -> Program
    findRoot' p = let parents = [ r | r <- xs,
                                  name p `isInfixOf` r,
                                  head (words r) /= name p ]
                      in if null parents
                         then p
                         else findRoot' (readProgram $ head parents)

buildTree :: Program -> [String] -> Program
buildTree root ss = let children = [ readProgram c | c <- ss,
                                     let this = head $ words c,
                                     let thisChildren = drop 3 . words $ string root,
                                     let onlyChars = filter (`elem` ['a'..'z']),
                                     this `elem` map onlyChars thisChildren ]
                    in Program
                       (name root)
                       (weight root)
                       (map (`buildTree` ss) children)
                       (string root)

weights :: Program -> ProgramWeight
weights p = if null $ carrying p
            then (weight p, weight p)
            else (weight p, weight p + (sum . map (snd . weights) $ carrying p))

balanced :: Program -> Bool
balanced p = let ws = map weights (carrying p)
             in all (== snd (head ws)) $ map snd ws

trav :: Program -> [Program]
trav root = root : concatMap trav (carrying root)

balance :: [ProgramWeight] -> Int
balance ws = let groups = groupBy (\x y -> snd x == snd y) ws
                 [bad:_, good:_] = sortBy (comparing length) groups
                 correction = snd bad - snd good
             in fst bad - correction
