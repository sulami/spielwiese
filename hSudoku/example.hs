{-# OPTIONS_GHC -O2 #-}

module Main where

import Sudoku

example =
 [4, 0, 6, 0, 0, 7, 0, 5, 0,
  3, 0, 0, 0, 4, 8, 1, 0, 0,
  0, 8, 0, 1, 0, 0, 6, 2, 0,
  7, 9, 2, 0, 6, 4, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 3, 0, 0,
  0, 0, 0, 7, 5, 0, 9, 6, 2,
  0, 7, 8, 0, 0, 3, 0, 9, 0,
  0, 0, 9, 8, 2, 0, 0, 0, 5,
  0, 6, 0, 5, 0, 0, 4, 0, 1]

main = do let ex = build example
          prettyPrint ex
          putStrLn "\n =>\n"
          prettyPrint $ solve ex

