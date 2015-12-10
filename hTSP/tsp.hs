-- Some programmatical thoughts about the travelling salesman problem.

-- Everything below is published under the ISC License, as usually. © 2015
-- Robin 'sulami' Schroer <sulami@peerwire.org>.

-- Disclaimer: This was before I found out Haskell actually has a proper data
-- type for graphs, so this is a bit ghetto, but works.

import Data.Ord (comparing)
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
    ("Faerun", [
      ("Tristram",       65),
      ("Tambi",         129),
      ("Norrath",       144),
      ("Snowdin",        71),
      ("Straylight",    137),
      ("AlphaCentauri",   3),
      ("Abre",          149)
    ]),
    ("Tristram", [
      ("Faerun",         65),
      ("Tambi",          63),
      ("Norrath",         4),
      ("Snowdin",       105),
      ("Straylight",    125),
      ("AlphaCentauri",  55),
      ("Abre",           14)
    ]),
    ("Tambi", [
      ("Faerun",        129),
      ("Tristram",       63),
      ("Norrath",        68),
      ("Snowdin",        52),
      ("Straylight",     65),
      ("AlphaCentauri",  22),
      ("Abre",          143)
    ]),
    ("Norrath", [
      ("Faerun",        144),
      ("Tristram",        4),
      ("Tambi",          68),
      ("Snowdin",         8),
      ("Straylight",     23),
      ("AlphaCentauri", 136),
      ("Abre",          115)
    ]),
    ("Snowdin", [
      ("Faerun",         71),
      ("Tristram",      105),
      ("Tambi",          52),
      ("Norrath",         8),
      ("Straylight",    101),
      ("AlphaCentauri",  84),
      ("Abre",           96)
    ]),
    ("Straylight", [
      ("Faerun",        137),
      ("Tristram",      125),
      ("Tambi",          65),
      ("Norrath",        23),
      ("Snowdin",       101),
      ("AlphaCentauri", 107),
      ("Abre",           14)
    ]),
    ("AlphaCentauri", [
      ("Faerun",          3),
      ("Tristram",       55),
      ("Tambi",          22),
      ("Norrath",       136),
      ("Snowdin",        84),
      ("Straylight",    107),
      ("Abre",           46)
    ]),
    ("Abre", [
      ("Faerun",        149),
      ("Tristram",       14),
      ("Tambi",         143),
      ("Norrath",       115),
      ("Snowdin",        96),
      ("Straylight",     14),
      ("AlphaCentauri",  46)
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
closest :: (Ord v) => [(k, v)] -> (k, v)
closest = L.minimumBy (comparing snd)

-- Here we just rotate a list.
rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]

-- This is a little helper that allows us to use two filters in one go.
both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
both a b x = a x && b x

-- This is a helper function that counts the number of occurences of an element
-- in a list of this elements type.
count' :: (Eq a) => a -> [a] -> Int
count' e = foldr (\m n -> if m == e then n + 1 else n) 0

-- Given a path in the form of a list of city names, calculate the length of
-- the path travelled.
pathLength :: (Eq k, Num v) => [k] -> [(k, [(k, v)])] -> v
pathLength p = pathLength' p 0
  where
    pathLength' :: (Eq k, Num v) => [k] -> v -> [(k, [(k, v)])] -> v
    pathLength' [x]    d l = d
    pathLength' (x:xs) d l = pathLength' xs
                                         (d + M.fromJust (dist x (head xs) l))
                                         l

-- This hack adds the first city again and also adds the distance from the
-- last back to the first city.
addLast :: (Eq k, Real v) => ([k], v) -> [(k, [(k, v)])] -> ([k], v)
addLast (c, n) l = (c ++ [head c], n + M.fromJust (dist (last c) (head c) l))

-- This will be the actual algorithm. It will start and end at the first city
-- in the map, because I am lazy like this.
tspNN :: (Eq k, Real v) => [(k, [(k, v)])] -> ([k], v)
tspNN l = addLast (nn ([fst (head l)], 0) l) l
  where
    nn :: (Eq k, Real v) => ([k], v) -> [(k, [(k, v)])] -> ([k], v)
    nn p           [x]    = p
    nn (prev, num) (x:xs) = nn (prev ++ [fst (next x xs)],
                                num + snd (next x xs))
                               (moveup (fst (next x xs)) xs)
    -- This returns the next city/distance to visit.
    next :: (Eq k, Real v) => (k, [(k, v)]) -> [(k, [(k, v)])] -> (k, v)
    next c l = closest $ filter' l (snd c)
    -- This rotates the list until the city we are looking for is in front.
    moveup :: (Eq k) => k -> [(k, [(k, v)])] -> [(k, [(k, v)])]
    moveup f (x:xs) | fst x == f = x : xs
                    |  otherwise = moveup f (xs ++ [x])
    -- This is a helper function that filters a list of city/distances tuples
    -- so that the returned list consists only of cities that are also in the
    -- second list of cities that have not been visited yet.
    filter' :: (Eq k) => [(k, [(k, v)])] -> [(k, v)] -> [(k, v)]
    filter' l = foldr (\x f -> if fst x `elem` [fst e | e <- l]
                               then f ++ [x] else f) []
-- As a next step we will rotate the map before running nearest neighbour so we
-- start at each city, and then chose the smallest distance traveled. This is a
-- O(n*n) = O(n^2) algorithm.
tspNNRot :: (Eq k, Real v) => [(k, [(k, v)])] -> ([k], v)
tspNNRot l = best $ tsp_nn_rot' [] (length l) l
  where
    -- We run nn once for every possible starting city...
    tsp_nn_rot' :: (Eq k, Real v) => [([k], v)] -> Int -> [(k, [(k, v)])]
                   -> [([k], v)]
    tsp_nn_rot' r 0 l = r
    tsp_nn_rot' r n l = tsp_nn_rot' (r ++ [tspNN l]) (n-1) (rotate l)
    -- ...and select the best result. For this we abuse the `closest` function
    -- which just happens to do the right thing already, because we only care
    -- about the second half of the tuples.
    best :: (Eq k, Real v) => [([k], v)] -> ([k], v)
    best = closest

-- Now on to something different, the definitve best solution to the problem,
-- at least in terms of the best result. We iterate through every possible
-- solution and choose the best one.
tspAll :: (Eq k, Ord k, Real v) => [(k, [(k, v)])] -> (v, [k])
tspAll l = let paths = map (\l -> l ++ [head l]) . L.permutations $ map fst l
               dists = map (`pathLength` l) paths
            in minimum $ zip dists paths

-- The next algorithm we will be looking at is the greedy algorithm. It will
-- try to accumulate the shortest possible edges to build the graph this way.
tspGreedy :: (Eq k, Real v) => [(k, [(k, v)])] -> ([k], v)
tspGreedy l = addLast (buildGraph (tsp_greedy' (buildList l) []) l) l
  where
    tsp_greedy' :: (Eq k, Real v) => [((k,k), v)] -> [((k,k), v)]
                   -> [((k,k), v)]
    tsp_greedy' [] r = r
    tsp_greedy' l  r = tsp_greedy'
                          (filter (filter''' (r ++ [best l]))
                            (filter (filter'' (r ++ [best l]))
                              (filter (filter' (best l)) l)))
                          (r ++ [best l])
    -- This function builds a list of possible vertices from the input dataset
    -- of edges and distances.
    buildList :: (Eq k, Real v) => [(k, [(k, v)])] -> [((k, k), v)]
    buildList = foldr (\(a,v) acc1 -> acc1 ++ foldr (\(b,d) acc2 ->
                                      acc2 ++ [((a,b), d)]) [] v) []
    -- Build the resulting graph from a list of edges. This assumes there is a
    -- graph spanning all vertices but not cycling. So we look for an edge with
    -- a vertex that is only used once and start constructing the graph from
    -- there until we used up all edges. Also, what do you know, closest comes
    -- in handy here.
    buildGraph :: (Eq k, Real v) => [((k, k), v)] -> [(k, [(k, v)])]
                  -> ([k], v)
    buildGraph e = dist' (buildGraph' [] (first' e) e)
      where
        buildGraph' :: (Eq k, Real v) => [k] -> k -> [((k, k), v)] -> [k]
        buildGraph' r c [] = r ++ [c]
        buildGraph' r c l  = buildGraph' (r ++ [c]) (next' l c)
                                       (filter (filter' ((c, next' l c), 0)) l)
        -- Find one of the vertices that is present only once.
        first' :: (Eq k, Real v) => [((k, k), v)] -> k
        first' l = fst . closest $ foldr (\e r -> r ++ [(e, count'' e l)]) []
                                         (get' l)
        -- Given one vertex, return the other/next one for constructing the
        -- graph.
        next' :: (Eq k) => [((k, k), v)] -> k -> k
        next' l k = head $ foldr (\e r -> if fst (fst e) == k
                                            then [snd (fst e)]
                                          else if snd (fst e) == k
                                            then [fst (fst e)]
                                          else r) [] l
        -- Calculate the distance traveled from a list of cities.
        dist' :: (Eq k, Real v) => [k] -> [(k, [(k, v)])] -> ([k], v)
        dist' k l = foldr (\e (k, d) -> (k ++ [e],
                                         d + M.fromJust (dist (last k) e l)))
                          (take 1 k, 0) (tail k)
    -- And again we can abuse closest to find the shortest possible edge in a
    -- list of edges.
    best :: (Real v) => [(k, v)] -> (k, v)
    best = closest
    -- The next component we will need a filter function that will remove both
    -- the edge we have added and the reverse on from the list of remaining
    -- possible ones.
    filter' :: (Eq k) => ((k, k), v) -> ((k, k), v) -> Bool
    filter' ((a1, b1), _) ((a2, b2), _) | a1 == a2 && b1 == b2 = False
                                        | a1 == b2 && b1 == a2 = False
                                        |            otherwise = True
    -- We also need a filter out the edges that involve at least one vertex
    -- that is already twice in the list of selected edges.
    filter'' :: (Eq k) => [((k, k), v)] -> ((k, k), v) -> Bool
    filter'' l ((a1, b1), _) | foldr ((\(a,b) n -> if (a1 == a) || (a1 == b)
                                then n + 1 else n) . fst) 0 l >= 2 = False
                             | foldr ((\(a,b) n -> if (b1 == a) || (b1 == b)
                                then n + 1 else n) . fst) 0 l >= 2 = False
                             |                                otherwise = True
    -- At last, we need to filter out any edge that would close the cycle
    -- before we have visitited every vertex/city. Therefore, we filter out all
    -- edges that would lead to a list of edges where every vertex is exactly
    -- twice in the list, thus we would have a cycle.
    filter''' :: (Eq k, Real v) => [((k, k), v)] -> ((k, k), v) -> Bool
    filter''' l ((a1, b1), _) = minimum (foldr
              (\e r -> r ++ [count'' e (l ++ [((a1, b1), 0)])])
              [] (get' (l ++ [((a1, b1), 0)]))) < 2
    -- Create a list of unique vertices from our data layout.
    get' :: (Eq k) => [((k, k), v)] -> [k]
    get' = L.nub . foldr ((\(a1, b1) r -> r ++ [a1, b1]) . fst) []
    -- This is a modified count version that counts the number of
    -- occurences of a vertex in a list of edges, specialized for our data
    -- layout.
    count'' :: (Eq k) => k -> [((k, k), v)] -> Int
    count'' a l = count' a (foldr ((\(a1, b1) r -> r ++ [a1, b1]) . fst) [] l)

