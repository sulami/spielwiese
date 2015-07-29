module Astar (
  Grid, Coord, Path,
  PossibleWaysFun, CostFun,
  flood
  ) where

import Control.Parallel.Strategies (parMap, rpar)
import GHC.Conc.Sync (par, pseq)

type Grid = [String]
type Coord = (Int, Int)
type Path = [Coord]

type PossibleWaysFun = Grid -> Coord -> Path
type CostFun = Coord -> Path -> Int

flood :: Grid -> Coord -> Coord -> PossibleWaysFun -> CostFun -> Path
flood grid fin pos pwf cf = head $ fl grid fin pwf cf [[pos]]
  where
    fl :: Grid -> Coord -> PossibleWaysFun -> CostFun -> [Path] -> [Path]
    fl grid fin pwf cf paths
      | any (\p -> last p == fin) paths = filter (\p -> last p == fin) paths
      | otherwise = let best = snd $ minimum
                               $ zip (parMap rpar (cf fin) paths) paths
                        pb = addRoutes grid paths best pwf
                    in fl grid fin pwf cf $ filter (/= best) paths ++ pb

    addRoutes :: Grid -> [Path] -> Path -> PossibleWaysFun -> [Path]
    addRoutes grid ps path pwf =
      [ path ++ [p] | p <- filter (`notElem` (concat ps)) $ pwf grid
                           $ last path ]

