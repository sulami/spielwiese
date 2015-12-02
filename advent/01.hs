module Main where

import Control.Monad (liftM)

step :: Int -> String -> IO Int
step (-1) _ = return 0
step f ('(':xs) = liftM (+1) $ step (f+1) xs
step f (')':xs) = liftM (+1) $ step (f-1) xs

main = do
  indata <- readFile "01"
  let up = length $ filter (== '(') indata
      dn = length $ filter (== ')') indata
  print $ up - dn
  rv <- step 0 indata
  print rv

