-- | Basic grid based lighting module.

module Light where

import Data.Function (on)

type Coord = (Int, Int)
type Cell = (Coord, Int)
type Grid = [[Cell]]
data LightSource = LightSource {
  lPosition :: Cell,
  lPower :: Int
} deriving (Show)

-- | Input the amount of light from each light source to get the combined
-- light.
lightInCell :: [Int] -> Int
lightInCell = maximum

-- | Add a specific amount of light to a cell.
addLight :: Int -> Cell -> Cell
addLight l (coords,x) = (coords, lightInCell [x,l])

-- | Automatically generate all the light values in a grid from a list of light
-- sources.
lightInGrid :: Grid -> [LightSource] -> Grid
lightInGrid g = foldr (\e r -> map (map (al e)) r) g
  where
    al :: LightSource -> Cell -> Cell
    al ls c = let amnt = lightFromSource c ls in addLight amnt c

--  Calculate the amount of light a cell gets from a source.
lightFromSource :: Cell -> LightSource -> Int
lightFromSource c s | c == lPosition s = lPower s
                    | otherwise = lPower s `div` distance c (lPosition s)

-- | Calculate the distance between two cells, rounded to an Int.
distance :: Cell -> Cell -> Int
distance ((x0,y0),_) ((x1,y1),_) = let a = ((abs (x0-x1)) ^ 2)
                                       b = ((abs (y0-y1)) ^ 2)
                                    in squareRoot $ a + b
  where squareRoot = round . sqrt . (fromIntegral :: Int -> Float)

