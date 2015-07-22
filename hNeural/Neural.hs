{-# OPTIONS_GHC -O2 #-}

module Neural (
  mkNeuNet, run,
  ) where

import Data.Matrix

type Data          = Matrix Double
type Synapse       = Matrix Double
data NeuralNetwork = NeuralNetwork {
  shape       :: [Int],
  weights     :: [Synapse]
} deriving (Show)

-- | Construct a new neural network with the shape supplied.
mkNeuNet :: [Int] -> NeuralNetwork
mkNeuNet s = NeuralNetwork s $ mkSyns $ zip (init s) (tail s)

mkSyns :: [(Int, Int)] -> [Synapse]
mkSyns = foldl (\r (y,x) -> r ++ [zero x $ y + 1]) []

-- | Run a set of data through all the layers of a neural network. This returns
-- all the intermediate steps. To only get the final result, use
--
-- > transpose $ last $ run ...
run :: NeuralNetwork -> Data -> [Data]
run net d0 = let indata = addBias $ transpose d0
              in scanl runLayer indata $ weights net
  where
    runLayer :: Data -> Synapse -> Data
    runLayer d s = predict s $ addBias d
    addBias :: Data -> Data
    addBias d = d <-> fromLists [[1 | c <- [1..(ncols d)]]]

dot :: Matrix Double -> Matrix Double -> Matrix Double
dot a b = fromLists [zipWith (*) (toList a) (toList b)]

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp(-x))

sigmoidDeriv :: Double -> Double
sigmoidDeriv x = x * (1 - x)

-- | Use the synapse to try to predict the result of the data.
predict :: Synapse -> Data -> Data
predict syn d1 = fmap sigmoid $ d1 `dot` syn

delta :: Data -> Data -> Data
delta want res = transpose $ (want - res) `dot` (fmap sigmoidDeriv res)

updWght :: Synapse -> Data -> Data -> Synapse
updWght syn d dlt = let mod = (transpose d) * dlt in syn + mod

train :: NeuralNetwork -> Data -> Data -> NeuralNetwork
train net input target = let result = run net input
                             dlt = delta d2 $ predict syn d1
                             ns = updWght syn d1 dlt
                          in scanr () result

