-- Largest prime factor
-- Problem 3
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143 ?

{-# OPTIONS_GHC -O2 #-}

divisors :: Integer -> [Integer]
divisors n = [x | x <- [2..(n `div` 2)], n `mod` x == 0] ++ [n]

pfd :: Integer -> [Integer]
pfd n = pfd' n []
  where
    pfd' :: Integer -> [Integer] -> [Integer]
    pfd' n r | product r == n = r
             |      otherwise = pfd' n (head (divs n r) : r)
    divs :: Integer -> [Integer] -> [Integer]
    divs n r = divisors (n `div` (product r))

main = print $ maximum $ pfd 600851475143

