{-# OPTIONS_GHC -O2 #-}

isPrime :: Integer -> Bool
isPrime n = [] == divisors n
  where
    divisors :: Integer -> [Integer]
    divisors n = [ x | x <- [3,5..(squareRoot n)], n `mod` x == 0]
    squareRoot :: Integer -> Integer
    squareRoot = ceiling . sqrt . (fromIntegral :: Integer -> Double)

rotate :: [a] -> [[a]]
rotate (x:xs) = let l = xs ++ [x] in l : rotate l

circularPrime :: Integer -> Bool
circularPrime n = let s = show n
                      p = map read $ take (length s) $ rotate s
                  in all odd p && all isPrime p

main = print $ length $ 2 : [x | x <- [3,5..999999], isPrime x, circularPrime x]

