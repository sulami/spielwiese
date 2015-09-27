module Main where

import           Test.QuickCheck (Arbitrary (arbitrary), quickCheck)

import           Game.DnD.Dice

main = do g <- initializeIO
          quickCheck (\d -> roll g (abs d) <= (abs d))
          quickCheck (\d -> roll g (abs d + 1) >= 1)

