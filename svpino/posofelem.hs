-- Playing around with svpino's post about finding the position of an element
-- in a list. Related post:
-- https://blog.svpino.com/2015/05/24/programming-challenge-the-position-of-the-element

-- This is the obvious solution that won't scale well with large arrays, O(n).
obvPos :: (Ord a) => a -> [a] -> Int
obvPos n l = find n l 0
  where
    find :: (Ord a) => a -> [a] -> Int -> Int
    find n [x]    c |    n <= x = c
                    | otherwise = c + 1
    find n (x:xs) c |    n <= x = c
                    | otherwise = find n xs (c+1)

-- Other idea: Try splitting the list in halfs to reduce the number of
-- comparisons down to O(log n).
splitPos :: (Ord a) => a -> [a] -> Int
splitPos n l = find n (l, 0)
  where
    find :: (Ord a) => a -> ([a], Int) -> Int
    find n ([x], c) |    n <= x = c
                    | otherwise = c + 1
    find n (l, c)   |  n <= head l = c
                    | length l > 1 = find n $ get n l c
                    |    otherwise = c
      where
        get :: (Ord a) => a -> [a] -> Int -> ([a], Int)
        get n l c | l !! (mid' l) == n = ([l !! (mid' l)], c + mid l - 1)
                  | l !! (mid' l) >  n = (take (mid l) l, c)
                  |          otherwise = (drop (mid l) l, c + mid l)
          where
            mid' :: [a] -> Int
            mid' l = (mid l) - 1
            mid :: [a] -> Int
            mid l = (length l) `div` 2
