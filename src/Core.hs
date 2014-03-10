module Core
(
  standardDeviation,
  variance,
  mean
) where

import Data.List

-- Public Functions

standardDeviation :: (Floating a) => [a] -> a
standardDeviation [] = 0
standardDeviation xs = sqrt $ variance xs

variance :: (Fractional a) => [a] -> a
variance [] = 0
variance xs = (1/genericLength xs) * (sum $ map subtractAndSquare xs)
    where subtractAndSquare x = (x - listMean) ^ 2
          listMean = mean xs


mean :: (Fractional a) => [a] -> a
mean [] = 0
mean xs = sum xs / genericLength xs
