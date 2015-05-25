-- Playing around with SVPINO's post about finding the position of an element
-- in an array. Related post:
-- https://blog.svpino.com/2015/05/24/programming-challenge-the-position-of-the-element

-- This is the obvious solution that won't scale well with large arrays, O(n).
obvPos :: Int -> [Int] -> Maybe Int
obvPos n l = find n l 0
  where
    find n [] c = Nothing
    find n l c | n == head l = Just c
               | otherwise = find n (tail l) (c+1)

