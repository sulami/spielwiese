-- Champernowne's constant
-- Problem 40
--
-- An irrational decimal fraction is created by concatenating the positive
-- integers:
--
-- 0.123456789101112131415161718192021...
--
-- It can be seen that the 12th digit of the fractional part is 1.
--
-- If dn represents the nth digit of the fractional part, find the value of
-- the following expression.
--
-- d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

{-# OPTIONS_GHC -O2 #-}

frac :: String
frac = concat . foldr (\e r -> show e : r) [] $ take 1000000 [1..]

digits :: Integral a => a -> [a]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

main = print $ product . digits . read . map (\e -> frac !! (e-1))
             $ [10^x | x <- [0..6]]

