-- Find out how many different states x bits can hold
bits :: Num a => Int -> a
bits x = 2^x

-- This is how this could be implemented using a lazily evaluated list
-- bins x | x > 0 = map (2^) [1..] !! (x-1)
--        | otherwise = 0

-- Find out how many different states x bytes can hold
byts :: Num a => Int -> a
byts x = bits $ x*8

-- The same for more 'bigger' bytes
bbyts :: Num a => Int -> Char -> a
bbyts x s | s == 'k'  = byts $ x*1024
          | s == 'M'  = bbyts (x*1024) 'k'
          | s == 'G'  = bbyts (x*1024) 'M'
          | otherwise = 0

