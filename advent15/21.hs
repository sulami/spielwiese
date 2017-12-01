module Main where

import           Data.Function (on)
import           Data.List     (groupBy)

weWin :: Int -> Int -> Int -> Int -> Bool
weWin php bhp dmg def
  | bhp <= 0  = True
  | php <= 0  = False
  | otherwise = weWin (php - 9 + def) (bhp - dmg + 2) dmg def

dmgCost :: Int -> Int
dmgCost  4 = 8
dmgCost  5 = 10
dmgCost  6 = 25
dmgCost  7 = 40
dmgCost  8 = 65
dmgCost  9 = 90
dmgCost 10 = 124
dmgCost 11 = 174

defCost :: Int -> Int
defCost 0 = 0
defCost 1 = 13
defCost 2 = 31
defCost 3 = 51
defCost 4 = 71
defCost 5 = 93
defCost 6 = 115
defCost 7 = 142
defCost 8 = 182

calcCost :: (Int, Int) -> Int
calcCost (dmg,def) = dmgCost dmg + defCost def

main = do
  let minDef = map head . groupBy ((==) `on` fst) $
                filter (uncurry (weWin 100 103))
                [ (dmg,def) | dmg <- [4..11], def <- [0..8] ]
  print . minimum $ map calcCost minDef

