module Main where

import Control.Arrow ((&&&))
import Data.List (groupBy, isPrefixOf, nub)
import Data.Function (on)

conc :: [(String, String)] -> (String, [String])
conc l = (fst (head l), map snd l)

walk :: ([a] -> [a] -> b) -> [a] -> [b]
walk f = walk' f []
  where
    walk' :: ([a] -> [a] -> b) -> [a] -> [a] -> [b]
    walk' _ _   []  = []
    walk' f bef aft = f bef aft : walk' f (bef ++ [head aft]) (tail aft)

repls :: String -> [(String, [String])] -> [String]
repls s rs = concat . filter (not . null) $ walk (search rs) s
  where
    search :: [(String, [String])] -> String -> String -> [String]
    search rs bef aft = concatMap (replace bef aft) $
      filter ((`isPrefixOf` aft) . fst) rs

    replace :: String -> String -> (String, [String]) -> [String]
    replace bef aft (bc,rs) = map (\r -> bef ++ r ++ drop (length bc) aft) rs

main = do
  indata <- lines <$> readFile "19.input"
  let replacements = map conc . groupBy ((==) `on` fst) . map ((head &&& last) .
        words) $ takeWhile (not . null) indata
      start = last indata
  print . length . nub $ repls start replacements

