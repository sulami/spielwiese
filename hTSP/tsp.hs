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
distances :: [(String, [(String, Integer)])]
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
  ]

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
closest :: (Real v) => [(k, v)] -> (k, v)
closest c = L.sortBy (\(a1, b1) (a2, b2) -> if      b1 < b2 then LT
                                            else if b1 > b2 then GT
                                            else                 EQ) c !! 0

-- Here we just rotate a list.
rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]

-- Given a path in the form of a list of city names, calculate the length of
-- the path travelled.
pathLength :: (Eq k, Num v) => [k] -> [(k, [(k, v)])] -> v
pathLength p l = pathLength' p 0 l
  where
    pathLength' :: (Eq k, Num v) => [k] -> v -> [(k, [(k, v)])] -> v
    pathLength' [x]    d l = d
    pathLength' (x:xs) d l = pathLength' xs
                                         (d + M.fromJust (dist x (head xs) l))
                                         l

-- This hack adds the first city again and also adds the distance from the
-- last back to the first city.
addLast :: (Eq k, Real v) => ([k], v) -> [(k, [(k, v)])] -> ([k], v)
addLast (c, n) l = (c ++ [c !! 0], n + (M.fromJust (dist (last c) (c !! 0) l)))

-- This will be the actual algorithm. It will start and end at the first city
-- in the map, because I am lazy like this.
tsp_nn :: (Eq k, Real v) => [(k, [(k, v)])] -> ([k], v)
tsp_nn l = addLast (nn ([fst (l !! 0)], 0) l) l
  where
    nn :: (Eq k, Real v) => ([k], v) -> [(k, [(k, v)])] -> ([k], v)
    nn p           [x]    = p
    nn (prev, num) (x:xs) = nn ((prev ++ [fst (next x xs)]),
                                num + (snd (next x xs)))
                               (moveup (fst (next x xs)) xs)
    -- This returns the next city/distance to visit.
    next :: (Eq k, Real v) => (k, [(k, v)]) -> [(k, [(k, v)])] -> (k, v)
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
-- As a next step we will rotate the map before running nearest neighbour so we
-- start at each city, and then chose the smallest distance traveled. This is a
-- O(n*n) = O(n^2) algorithm.
tsp_nn_rot :: (Eq k, Real v) => [(k, [(k, v)])] -> ([k], v)
tsp_nn_rot l = best $ tsp_nn_rot' [] (length l) l
  where
    -- We run nn once for every possible starting city...
    tsp_nn_rot' :: (Eq k, Real v) => [([k], v)] -> Int -> [(k, [(k, v)])]
                   -> [([k], v)]
    tsp_nn_rot' r 0 l = r
    tsp_nn_rot' r n l = tsp_nn_rot' (r ++ [tsp_nn l]) (n-1) (rotate l)
    -- ...and select the best result. For this we abuse the `closest` function
    -- which just happens to do the right thing already, because we only care
    -- about the second half of the tuples.
    best :: (Eq k, Real v) => [([k], v)] -> ([k], v)
    best = closest

-- Now on to something different, the definitve best solution to the problem,
-- at least in terms of the best result. We iterate through every possible
-- solution and choose the best one. Runtime is of course abysmal, O(n!). This
-- is also quite a tricky one because of the double recursion. Because we
-- supply the first city to visit, we have to remove the first list element
-- afterwards. We also have to run this after a rotation so we start at every
-- possible city.
tsp_all :: (Eq k, Real v) => [(k, [(k, v)])] -> ([k], v)
tsp_all l = best $ map (`addLast` l) (addDistances (rotate' l [] (length l)) l)
  where
    tsp_all' :: (Eq k, Real v) => [[k]] -> [k] -> [(k, [(k, v)])] -> Int
                -> [[k]]
    tsp_all' r p []     _ = r ++ [p]
    tsp_all' r p _      0 = r
    tsp_all' r p (x:xs) c = tsp_all' (tsp_all' r (p ++ [(fst x)]) xs
                                (length xs)) p (rotate (x:xs)) (c-1)
    -- This runs the entire algorithm for every starting city.
    rotate' :: (Eq k, Real v) => [(k, [(k, v)])] -> [[k]] -> Int -> [[k]]
    rotate' _ r 0 = r
    rotate' l r c = rotate' (rotate l) (r ++ (tail (tsp_all' [[fst (head l)]]
                                  [fst (head l)] (tail l) (length l)))) (c-1)
    -- After generating all possible paths, we calculate the length of each.
    addDistances :: (Eq k, Real v) => [[k]] -> [(k, [(k, v)])] -> [([k], v)]
    addDistances p l = foldl (\r x -> r ++ [(x, pathLength x l)]) [] p
    -- Again, we need to get the best result we have calculated.
    best :: (Eq k, Real v) => [([k], v)] -> ([k], v)
    best = closest

-- The next algorithm we will be looking at is the greedy algorithm. It will
-- try to accumulate the shortest possible edges to build the graph this way.

-- tsp_greedy :: (Eq k, Real v) => [(k, [(k, v)])] -> ([k], v)
-- This function builds a list of possible vertices from the input dataset of
-- edges and distances.
buildList :: (Eq k, Real v) => [(k, [(k, v)])] -> [((k, k), v)]
buildList l = foldl (\acc1 (a,v) ->
                      acc1 ++ (foldl (\acc2 (b,d) ->
                      acc2 ++ [((a,b), d)]) [] v)) [] l
-- And again we can abuse closest to find the shortest possible edge in a list
-- of edges.
best :: (Real v) => [(k, v)] -> (k, v)
best = closest
-- The last component we will need a filter function that will remove both the
-- edge we have added and the reverse on from the list of remaining possible
-- ones.
filter' :: (Eq k, Real v) => (k, k) -> ((k, k), v) -> Bool
filter' (a1, b1) ((a2, b2), _) | a1 == a2 && b1 == b2 = False
                               | a1 == b2 && b1 == a2 = False
                               |            otherwise = True

