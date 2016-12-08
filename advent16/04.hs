module Main where

import           Data.List (elemIndices, isInfixOf, nub, sort, sortBy)
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

decypher :: Room -> Room
decypher (name, cs, sid) =
  let allchars = cycle ['a'..'z']
      translate c = allchars !! (sid + head (elemIndices c allchars))
  in (map translate name, cs, sid)

findNPOs :: Room -> Bool
findNPOs (name, _, _) = "northpoleobject" `isInfixOf` name

thrd :: (a, b, c) -> c
thrd (_, _, x) = x

main :: IO ()
main = do
  indata <- map readRoom . lines <$> readFile "04.input"
  let realRooms = filter checkChecksum indata
  print . sum $ map thrd realRooms
  print . thrd . head . filter findNPOs $ map decypher realRooms
