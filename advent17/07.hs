#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

import           Data.List (isInfixOf)

data Program = Program
  { name     :: String
  , weight   :: Int
  , carrying :: [Program]
  , string   :: String
  } deriving Show

readProgram :: String -> Program
readProgram s = let (n:ws:_) = words s
                    w = read $ filter (`elem` ['0'..'9']) ws
                in Program n w [] s

showProgram :: Program -> String
showProgram = showProgram' ""
  where
    showProgram' :: String -> Program -> String
    showProgram' prefix p = let self = concat [prefix, name p, " (", show (weight p), ")"]
                                children = map (showProgram' (prefix ++ " ")) $ carrying p
                            in unlines $ self : [concat children]

main :: IO ()
main = do
  input <- lines <$> getContents
  let root = findRoot input
  putStrLn $ name root
  let tree = buildTree root input
      imbalanced = last . filter (not. balanced) $ trav tree
  print . map weights $ carrying imbalanced

findRoot :: [String] -> Program
findRoot []     = undefined
findRoot (x:xs) = findRoot' (readProgram x) xs
  where
    findRoot' :: Program -> [String] -> Program
    findRoot' p [] = p
    findRoot' p ss = let parents = [ r | r <- ss,
                                     name p `isInfixOf` r,
                                     head (words r) /= name p ]
                      in if null parents
                         then p
                         else findRoot' (readProgram $ head parents) ss

buildTree :: Program -> [String] -> Program
buildTree root ss = let children = [ readProgram c | c <- ss,
                                     head (words c) `elem` map (filter (`elem` ['a'..'z'])) (drop 3 (words (string root))) ]
                    in Program (name root) (weight root) (map (`buildTree` ss) children) (string root)

weights :: Program -> (Int, Int)
weights p = if null $ carrying p
            then (weight p, weight p)
            else (weight p, weight p + (sum . map (snd . weights) $ carrying p))

balanced :: Program -> Bool
balanced p = let ws = map weights (carrying p) in all (== snd (head ws)) $ map snd ws

trav :: Program -> [Program]
trav root = root : concatMap trav (carrying root)
