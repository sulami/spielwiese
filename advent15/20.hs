module Main where

elves = map (\e -> take 10 [e,e*2..]) [1..]
houses = map (\h -> sum . map last . filter (elem h) $ take h elves) [1..]

main = do
  let target = 36000000
  print . (+1) . length $ takeWhile (< target) houses

