module VirtualArrow.Utils
(
    frequences
) where

import qualified Data.Map.Strict as Map

frequences :: (Ord a) => [a] -> [(a, Int)]
frequences list = Map.toList $ Map.fromListWith (+) [(c, 1) | c <- list]
