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

main :: IO ()
main = do
  input <- lines <$> getContents
  let tree = buildTree input
  putStrLn $ name tree

buildTree :: [String] -> Program
buildTree []     = undefined
buildTree (x:xs) = buildTree' (readProgram x) xs
  where
    buildTree' :: Program -> [String] -> Program
    buildTree' p [] = p
    buildTree' p ss = let parents = [ r | r <- ss,
                                          name p `isInfixOf` r,
                                          head (words r) /= name p ]
                      in if null parents
                         then p
                         else buildTree' (readProgram $ head parents) ss
