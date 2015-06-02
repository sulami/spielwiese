-- Some programmatical thoughts about the travelling salesman problem.

-- Everything below is published under the ISC License, as usually. © 2015
-- Robin 'sulami' Schroer <sulami@peerwire.org>.

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



