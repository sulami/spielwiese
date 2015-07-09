module Riemann (riemannSum) where

-- Calculates the Riemann Sum, using a function term, the limits of the
-- integration and the number of rectangles to use.
riemannSum :: (Double -> Double) -> Double -> Double -> Double -> Double
riemannSum f a b n = sum [f (a + i * dx) | i <- [0..n-1]] * dx
  where
    dx = (b - a) / n

