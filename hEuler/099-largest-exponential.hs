-- Largest exponential
-- Problem 99
--
-- Comparing two numbers written in index form like 2^11 and 3^7 is not
-- difficult, as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.
--
-- However, confirming that 632382^518061 > 519432^525806 would be much more
-- difficult, as both numbers contain over three million digits.
--
-- Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K text
-- file containing one thousand lines with a base/exponent pair on each line,
-- determine which line number has the greatest numerical value.
--
-- NOTE: The first two lines in the file represent the numbers in the example
-- given above.

{-# OPTIONS_GHC -O2 #-}

split :: [String] -> String -> [String]
split r (x:xs) |  x == ',' = r ++ [xs]
               | otherwise = split (app r [x]) xs
  where
    app [] e = [e]
    app l  e = (init l) ++ [last l ++ e]

prep :: String -> [(Integer, (Integer, Integer))]
prep = (zip [1..]) . (map conv) . (map (split [])) . lines
  where
    conv :: [String] -> (Integer, Integer)
    conv (a:b:_) = (read a, read b)

calc :: (Integer, (Integer, Integer)) -> (Integer, Integer)
calc (n, (a,b)) = (a^b, n)

main = do input <- readFile "099.input"
          print $ maximum . (map calc) . prep $ input

