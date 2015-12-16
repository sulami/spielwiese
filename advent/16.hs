module Main where

import Data.Maybe (fromJust)

readData :: String -> [(String, Int)]
readData l = let fl = filter (not . (`elem` [':',','])) l
                 ds = drop 2 $ words fl
              in parseData ds
  where
    parseData :: [String] -> [(String, Int)]
    parseData []      = []
    parseData (x:y:z) = (x, read y) : parseData z

match :: [(String, Int)] -> (Int, [(String, Int)]) -> Bool
match want = all (\(k,v) -> v == fromJust (lookup k want)) . snd

main = do
  indata <- lines <$> readFile "16.input"
  let want = [("children",3),
              ("cats",7),
              ("samoyeds",2),
              ("pomeranians",3),
              ("akitas",0),
              ("vizslas",0),
              ("goldfish",5),
              ("trees",3),
              ("cars",2),
              ("perfumes",1)]
  print . filter (match want) . zip [1..] $ map readData indata

