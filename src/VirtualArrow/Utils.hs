module VirtualArrow.Utils
(
    frequences, minIndex
) where

import qualified Data.Map.Strict as Map

frequences :: (Ord a) => [a] -> [(a, Int)]
frequences list = Map.toList $ Map.fromListWith (+) [(c, 1) | c <- list]

minIndex :: (Ord a) => [a] -> Int
minIndex list = snd . minimum $ zip list [0 .. ]
