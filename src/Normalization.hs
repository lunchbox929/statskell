module Normalization
(
	allZScores,
	zScore
) where

import Core (standardDeviation, mean)

-- Public Functions

-- This function takes a list of values and returns the list of zscores
-- Order is maintained
allZScores :: (Floating a) => [a] -> [a]
allZScores [] = []
allZScores [a] = [a]
allZScores xs = map getZ xs
 	where getZ = zScore (mean xs) (standardDeviation xs)

-- This will take a Score, a Mean, and a Standard Deviation and return the
-- Z Score of the Score parameter
zScore :: (Floating a) => a -> a -> a -> a
aScore _ 0 _ = error "You can't calculate the z score when the standard deviation is 0"
zScore mean stdDev score = (score - mean) / stdDev
