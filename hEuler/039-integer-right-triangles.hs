-- Integer right triangles
-- Problem 39
--
-- If p is the perimeter of a right angle triangle with integral length sides,
-- {a,b,c}, there are exactly three solutions for p = 120.
--
-- {20,48,52}, {24,45,51}, {30,40,50}
--
-- For which value of p ≤ 1000, is the number of solutions maximised?
--
{-# OPTIONS_GHC -O2 #-}

triangle :: Int -> Int
triangle n = length $ [(a,b,c) | a <- [1..(n-2)],
                                 b <- [1..(n-2)],
                                 c <- [2..(n-2)],
                                 b >= a,
                                 a + b + c == n,
                                 a^2 + b^2 == c^2]

main = print $ snd . maximum $ zip (map triangle [1..1000]) [1..1000]

