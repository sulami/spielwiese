-- Do some calculations regarding the happy sequence.
--
-- To get the next item in the happy sequence, square each digit of the last
-- item and add them up. The list either goes to 10 then 1 and stays there, or
-- goes on infinitely, for example when starting with 42, which returns to 42
-- after 8 iterations.

happy' :: String -> Int
happy' (x:xs) = (read [x] :: Int) ^ (2 :: Int) + happy' xs
happy' x = 0

-- Generate the next value of the sequence after x.
happy :: Int -> Int
happy x = happy' $ show x

happySequence' :: Int -> [Int] -> [Int]
happySequence' n r | n > 0     = happySequence' (n-1) (r ++ [happy (last r)])
                   | otherwise = r

-- Generate a happy sequence of n elements starting with s.
happySequence :: Int -> Int -> [Int]
happySequence n s = happySequence' n [s]

