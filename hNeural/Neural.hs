{-# OPTIONS_GHC -O2 #-}

module Neural where

import Data.Matrix

type Data    a = Matrix a
type Synapse a = Matrix a

dot :: Matrix Double -> Matrix Double -> Matrix Double
dot a b = fromLists [zipWith (*) (toList a) (toList b)]

madd :: Matrix Double -> Matrix Double -> Matrix Double
madd a b = fromLists [zipWith (+) (toList a) (toList b)]

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp(-x))

sigmoidDeriv :: Double -> Double
sigmoidDeriv x = x * (1 - x)

predict :: Data Double -> Synapse Double -> Data Double
predict d syn = fmap sigmoid $ multStd2 d syn

delta :: Data Double -> Data Double -> Data Double
delta want res = transpose $ (want - res) `dot` (fmap sigmoidDeriv res)

updWght :: Synapse Double -> Data Double -> Data Double -> Synapse Double
updWght syn d dlt = let mod = (transpose d) * dlt in syn + mod

train :: Synapse Double -> Data Double -> Data Double -> Int -> Synapse Double
train syn _  _  0 = syn
train syn d1 d2 c = let dlt = delta d2 $ predict d1 syn
                        ns = updWght syn d1 dlt
                     in train ns d1 d2 (c-1)

