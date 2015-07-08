{-# OPTIONS_GHC -O2 #-}

module Main where

import System.Environment (getArgs)

rev :: (Integer, String) -> String
rev (n, s) |     odd n = reverse s
           | otherwise = s

main = do arg <- fmap (unwords) getArgs
          let out = unwords $ map rev $ zip [0..] $ words
                    $ takeWhile (/= '.') arg
          putStrLn $ out ++ "."

