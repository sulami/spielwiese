-- | Basic grid based lighting module.

module Light where

type Cell = (Int, Int)
type Grid = [[Cell]]
data LightSource = LightSource {
  lPosition :: Cell,
  lPower :: Int
} deriving (Show)

-- | Input the amount of light from each light source to get the combined
-- light.
lightInCell :: [Int] -> Int
lightInCell = maximum

-- | Calculate the amount of light a cell gets from a source.
lightFromSource :: Cell -> LightSource -> Int
lightFromSource c s | c == lPosition s = lPower s
                    | otherwise = lPower s `div` distance c (lPosition s)

-- | Calculate the distance between two cells, rounded to an Int.
distance :: Cell -> Cell -> Int
distance (x0,y0) (x1,y1)= squareRoot $ ((abs (x0-x1)) ^ 2) + ((abs (y0-y1)) ^ 2)
  where squareRoot = round . sqrt . (fromIntegral :: Int -> Float)

