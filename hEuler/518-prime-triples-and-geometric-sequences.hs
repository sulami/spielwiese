{-# OPTIONS_GHC -O2 #-}

isPrime :: Integer -> Bool
isPrime n = [] == [x | x <- [2..(n `div` 2)], n `mod` x == 0]

primes :: [Integer]
primes = [x | x <- [2..100000000], isPrime x]

isWhole :: RealFrac a => a -> Bool
isWhole n = floor n == ceiling n

getC :: Integer -> Integer -> [Integer]
getC a b = [round x | x <- [ca], x <= 100000000, isWhole x]
  where
    ca = b1 / a1 * b1 - 1
    a1 = fromInteger $ a + 1
    b1 = fromInteger $ b + 1

main = print $ sum [a+b+c | a <- primes,
                            b <- [x | x <- primes, x > a],
                            c <- [x | x <- getC a b, x `elem` primes]
                            ]

