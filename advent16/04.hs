module Main where

import           Data.List (elemIndices, nub, sort, sortBy)
import           Data.Ord  (comparing)

type Room = (String, String, Int)

readRoom :: String -> Room
readRoom raw = let (start, end) = break (== '[') raw
                   name = filter (`elem` ['a'..'z']) start
                   checksum = tail $ init end
                   sid = read $ filter (`elem` ['0'..'9']) start
                in (name, checksum, sid)

calcChecksum :: String -> String
calcChecksum input = take 5 . sortBy mostOccurrences . nub $ sort input
  where
    mostOccurrences :: Char -> Char -> Ordering
    mostOccurrences = flip (comparing numOccurences)

    numOccurences :: Char -> Int
    numOccurences c = length $ elemIndices c input

checkChecksum :: Room -> Bool
checkChecksum (name, cs, _) = cs == calcChecksum name

thrd :: (a, b, c) -> c
thrd (_, _, x) = x

main :: IO ()
main = do
  indata <- map readRoom . lines <$> readFile "04.input"
  print . sum . map thrd $ filter checkChecksum indata
