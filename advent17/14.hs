#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc

module Main where

import           Data.Bits (xor)
import           Data.Char (intToDigit, ord)
import           Numeric   (showIntAtBase)

main :: IO ()
main = do
  input <- getLine
  let strings = take 128 $ map (\n -> input ++ "-" ++ show n) ([0..] :: [Int])
  print . sum $ concatMap (map (read . (:[]) :: Char -> Int) . knotHash) strings

showBinary :: Int -> String
showBinary x = let bin = showIntAtBase 2 intToDigit x ""
                   prefix = replicate (4 - length bin) '0'
               in prefix ++ bin

-- Knot Hash code from day 10
type State = (Int, Int, [Int])

knotHash :: String -> String
knotHash input = let input2 = reverse . (++ [17, 31, 73, 47, 23]) $ map ord input
                     sparse = thrd . (!! 64) $ iterate (\st -> foldr step st input2) (0, 0, [0..255])
                 in concatMap (showBinary . foldr1 xor) $ segment 16 sparse

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

segment :: Int -> [a] -> [[a]]
segment _ [] = []
segment i xs = let (h, t) = splitAt i xs in h : segment i t
