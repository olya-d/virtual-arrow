module VirtualArrow.Utils
(
    frequencies, minIndex, maxIndex, (/.), count
) where

import qualified Data.Map.Strict as Map
import Data.Function (on)

frequencies :: (Ord a) => [a] -> [(a, Int)]
frequencies list = Map.toList $ Map.fromListWith (+) [(c, 1) | c <- list]

minIndex :: (Ord a) => [a] -> Int
minIndex list = snd . minimum $ zip list [0 .. ]

maxIndex :: (Ord a) => [a] -> Int
maxIndex list = snd . minimum $ zip list [0 .. ]

(/.) :: Int -> Int -> Double
(/.) = (/) `on` fromIntegral

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f