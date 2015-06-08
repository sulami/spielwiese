-- Some programmatical thoughts about the travelling salesman problem.

-- Everything below is published under the ISC License, as usually. © 2015
-- Robin 'sulami' Schroer <sulami@peerwire.org>.

import qualified Data.List as L
import qualified Data.Maybe as M

-- The input data is taken from a friend's assignment and features six German
-- cities. The problem is the typical TSP, visit every city excactly once while
-- optimizing the total distance travelled. The start- and endvertex can be
-- chosen freely, but must be the same, so we get a Hamiltonian cycle. The
-- distance given is measured in kilometres and taken directly from the
-- assignment, so it might be factually wrong.

-- To represent the distances between the vertices, we will use a map of maps.
-- The city names do not use German umlauts for compability reasons (mainly
-- with my qwerty keyboard).
distances =
  [
    ("Aachen", [
      ("Bonn",        91),
      ("Duesseldorf", 80),
      ("Frankfurt",  259),
      ("Koeln",       70),
      ("Wuppertal",  121)
    ]),
    ("Bonn", [
      ("Aachen",      91),
      ("Duesseldorf", 77),
      ("Frankfurt",  175),
      ("Koeln",       27),
      ("Wuppertal",   84)
    ]),
    ("Duesseldorf", [
      ("Aachen",      80),
      ("Bonn",        77),
      ("Frankfurt",  232),
      ("Koeln",       47),
      ("Wuppertal",   29)
    ]),
    ("Frankfurt", [
      ("Aachen",     259),
      ("Bonn",       175),
      ("Duesseldorf",232),
      ("Koeln",      189),
      ("Wuppertal",  236)
    ]),
    ("Koeln", [
      ("Aachen",      70),
      ("Bonn",        27),
      ("Duesseldorf", 47),
      ("Frankfurt",  189),
      ("Wuppertal",   55)
    ]),
    ("Wuppertal", [
      ("Aachen",     121),
      ("Bonn",        84),
      ("Duesseldorf", 29),
      ("Frankfurt",  236),
      ("Koeln",       55)
    ])
  ] :: [(String, [(String, Integer)])]

-- To make things easier, we write a custom lookup function, so we can easily
-- get the distance between two cities without having to mess with
-- Data.Map.lookup directly. This is a textbook recursive O(n+(n-1)) search
-- loop, but that does not matter since our dataset is really small.
dist :: (Eq k, Num v) => k -> k -> [(k, [(k, v)])] -> Maybe v
dist a b = foldr (\(k, v) no -> if a == k then dist' b v else no) Nothing
  where
    dist' :: (Eq k, Num v) => k -> [(k, v)] -> Maybe v
    dist' b = foldr (\(k,v) no -> if b == k then Just v else no) Nothing

-- We also write a lookup funktion to get the inner map of one of the cities.
get :: (Eq k, Num v) => k -> [(k, [(k, v)])] -> Maybe [(k, v)]
get c = foldr (\(k, v) no -> if c == k then Just v else no) Nothing

-- Now on to some traveling. The first algorithm we will be using is nearest
-- neighbour. We will start in one city and always go to the closest city that
-- we have not visited yet. We will return a list of cities we have visited in
-- order and the total distance traveled. Nearest neighbour is of course O(n).

-- For this, we will be using this function that returns the closest city to
-- another city given, chosen from a list of city/distance sets.
closest :: (Eq k, Ord v, Num v) => [(k, v)] -> (k, v)
closest c = L.sortBy (\(a1, b1) (a2, b2) -> if      b1 < b2 then LT
                                            else if b1 > b2 then GT
                                            else                 EQ) c !! 0

-- This will be the actual algorithm. It will start and end at the first city
-- in the map, because I am lazy like this.
tsp_nn :: (Eq k, Ord v, Num v) => [(k, [(k, v)])] -> ([k], v)
tsp_nn l = addLast (nn ([fst (l !! 0)], 0) l) l
  where
    nn :: (Eq k, Ord v, Num v) => ([k], v) -> [(k, [(k, v)])] -> ([k], v)
    nn p           [x]    = p
    nn (prev, num) (x:xs) = nn ((prev ++ [fst (next x xs)]),
                                num + (snd (next x xs)))
                               (moveup (fst (next x xs)) xs)
    -- This returns the next city/distance to visit.
    next :: (Eq k, Ord v, Num v) => (k, [(k, v)]) -> [(k, [(k, v)])] -> (k, v)
    next c l = closest $ filter' (snd c) l
    -- This rotates the list until the city we are looking for is in front.
    moveup :: (Eq k) => k -> [(k, [(k, v)])] -> [(k, [(k, v)])]
    moveup f (x:xs) | fst x == f = [x] ++ xs
                    |  otherwise = moveup f (xs ++ [x])
    -- This is a helper function that filters a list of city/distances tuples
    -- so that the returned list consists only of cities that are also in the
    -- second list of cities that have not been visited yet.
    filter' :: (Eq k) => [(k, v)] -> [(k, [(k, v)])] -> [(k, v)]
    filter' xs l = foldl (\f x -> if fst x `elem` [fst e | e <- l]
                                  then f ++ [x] else f) [] xs
    -- This hack adds the first city again and also adds the distance from the
    -- last back to the first city.
    addLast :: (Eq k, Ord v, Num v) => ([k], v) -> [(k, [(k, v)])] -> ([k], v)
    addLast (c, n) l = (c ++ [c !! 0],
                        n + (M.fromJust (dist (last c) (c !! 0) l)))

-- As a next step we will rotate the map before running nearest neighbour so we
-- start at each city, and then chose the smallest distance traveled. This is a
-- O(n*n) = O(n^2) algorithm.
tsp_nn_rot :: (Eq k, Ord v, Num v) => [(k, [(k, v)])] -> ([k], v)
tsp_nn_rot l = best $ tsp_nn_rot' [] (length l) l
  where
    -- We run nn once for every possible starting city...
    tsp_nn_rot' :: (Eq k, Ord v, Num v) => [([k], v)] -> Int -> [(k, [(k, v)])]
                   -> [([k], v)]
    tsp_nn_rot' r 0 l = r
    tsp_nn_rot' r n l = tsp_nn_rot' (r ++ [tsp_nn l]) (n-1) (rotate l)
    -- ...and select the best result. For this we abuse the `closest` function
    -- which just happens to do the right thing already, because we only care
    -- about the second half of the tuples.
    best :: (Eq k, Ord v, Num v) => [([k], v)] -> ([k], v)
    best l = closest l
    -- Here we just rotate a list.
    rotate :: [a] -> [a]
    rotate (x:xs) = xs ++ [x]

-- Now on to something different, the definitve best solution to the problem,
-- at least in terms of the best result. We iterate through every possible
-- solution and choose the best one. Runtime is of course abysmal, O(n!). This
-- is also quite a tricky one because of the double recursion.
tsp_all :: (Eq k, Ord v, Num v) => [(k, [(k, v)])] -> [([k], v)]
tsp_all l = tsp_all' [([fst (head l)], 0)] (tail l) (length l)
  where
    tsp_all' :: (Eq k, Ord v, Num v) => [([k], v)] -> [(k, [(k, v)])] -> Int -> [([k], v)]
    tsp_all' r []     _ = r
    tsp_all' r _      0 = r
    -- This will be the inner loop, that will take the first unvisited city and
    -- visit it. What still needs to be done is properly incrementing the
    -- distance travelled.
    tsp_all' r (x:xs) c = (tsp_all'
                              (r ++ [( fst (last r) ++ [(fst x)] , snd (last r) + 1)])
                              xs
                              (length xs)
                            )
    -- This will be the outer loop that will rotate the list of unvisited
    -- cities.
    -- tsp_all' r (x:xs) c = tsp_all' <INNER LOOP> (rotate (x:xs)) (c-1)
    -- TODO Later on we need to filter the set of results to only use complete
    -- travels, then add the last travel back like before and then choose the
    -- best route.
    rotate :: [a] -> [a]
    rotate (x:xs) = xs ++ [x]
    -- best :: (Eq k, Ord v, Num v) => [([k], v)] -> ([k], v)
    -- best l = closest l

