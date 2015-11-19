module Main where

blocks :: String
blocks = " ▁▂▃▄▅▆▇█"

normalize :: [Int] -> [Int]
normalize a = map (\n -> 8 * n `div` (maximum a)) a

visualize :: [Int] -> String
visualize = map (\n -> blocks !! n) . normalize

main :: IO ()
main = visualize . map read . words <$> getLine >>= putStrLn

