{-# OPTIONS_GHC -O2 #-}

module Main where

import Data.Matrix  (fromLists)

import Neural       (mkNeuNet, run)

net = mkNeuNet [2,2,1]
x = fromLists $ [[0,0],[1,1],[-1,0.5]]

main = print $ run net x

