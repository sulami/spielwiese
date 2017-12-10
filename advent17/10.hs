#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

type State = (Int, Int, [Int])

main :: IO ()
main = do
  input <- reverse . read . ('[' :) . (++ "]") <$> getLine
  print . solve $ foldr step (0, 0, [0..255]) input

solve :: State -> Int
solve = (\[x, y] -> x * y) . take 2 . thrd

step :: Int -> State -> State
step len (pos, skip, s0) = let newPos = (pos + len + skip) `mod` length s0
                               s1 = rev pos len s0
                           in (newPos, skip + 1, s1)

rev :: Int -> Int -> [Int] -> [Int]
rev pos len s0 = let s0' = cycle s0
                     (pref, post) = splitAt pos s0'
                     (toRev, rest) = splitAt len post
                     newPost = reverse toRev ++ rest
                     infi = pref ++ newPost
                     desiredLen = length s0
                     toShift = pos + len - desiredLen
                 in if toShift > 0
                    then shift toShift . take desiredLen $ drop toShift infi
                    else take desiredLen infi

shift :: Int -> [a] -> [a]
shift _ [] = []
shift 0 xs = xs
shift n xs = shift (n - 1) $ last xs : init xs

thrd :: (a, b, c) -> c
thrd (_, _, x) = x
