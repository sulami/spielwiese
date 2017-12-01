module Main where

import Control.Arrow ((&&&))
import Data.List (maximumBy)
import Data.Ord (comparing)

type Ingridient = (Int, Int, Int, Int, Int)

propSums :: [(Int, Ingridient)] -> [Int]
propSums [] = repeat 0
propSums ((n,(cap,dur,fla,tex,cal)):is) =
  zipWith (+) (propSums is) $ map (*n) [cap,dur,fla,tex]

score :: [(Int, Ingridient)] -> Int
score is = product . map (max 0) $ propSums is

cals :: [(Int, Ingridient)] -> Int
cals is = sum $ map (\(n,(_,_,_,_,cal)) -> cal*n) is

main = do
  let is = [(4,-2,0,0,5), (0,5,-1,0,8), (-1,0,5,0,6), (0,0,-2,2,1)]
      ns = [ zip [a,b,c,100-a-b-c] is | a <- [1..97],
                                        b <- [1..97],
                                        c <- [1..97],
                                        a+b+c <= 99 ]
  print . maximum $ map score ns
  print . snd . maximumBy (comparing snd) . filter ((== 500) . fst) $
    map (cals &&& score) ns

