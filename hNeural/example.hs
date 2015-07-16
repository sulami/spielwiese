{-# OPTIONS_GHC -O2 #-}

module Main where

import Data.Matrix (fromLists, toList, transpose)
import Neural (train, predict)

syn0 = fromLists $ [[-0.16595599], [ 0.44064899], [-0.99977125]]
x = fromLists $ [[1,0,1], [0,1,1], [1,1,0], [1,1,1], [0,1,0], [1,0,0], [0,0,1]]
y = transpose $ fromLists $ [[1,1,0,1,0,0,1]]

main = do let syn1 = train syn0 x y 100000
          print $ predict syn1 (fromLists [[0,0,1]])

