{-# OPTIONS_GHC -O2 #-}

module Neural (
  predict, train
  ) where

import Data.Matrix

type Data    a = Matrix a
type Synapse a = Matrix a

dot :: Matrix Double -> Matrix Double -> Matrix Double
dot a b = fromLists [zipWith (*) (toList a) (toList b)]

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp(-x))

sigmoidDeriv :: Double -> Double
sigmoidDeriv x = x * (1 - x)

-- | Use the synapse to try to predict the result of the data.
predict :: Synapse Double -> Data Double -> Data Double
predict syn d1 = fmap sigmoid $ multStd2 d1 syn

delta :: Data Double -> Data Double -> Data Double
delta want res = transpose $ (want - res) `dot` (fmap sigmoidDeriv res)

updWght :: Synapse Double -> Data Double -> Data Double -> Synapse Double
updWght syn d dlt = let mod = (transpose d) * dlt in syn + mod

-- | Use a base synapse, input data and expected output to train the synapse
-- for c iterations.
train :: Synapse Double -> Data Double -> Data Double -> Int -> Synapse Double
train syn _  _  0 = syn
train syn d1 d2 c = let dlt = delta d2 $ predict syn d1
                        ns = updWght syn d1 dlt
                     in train ns d1 d2 (c-1)

