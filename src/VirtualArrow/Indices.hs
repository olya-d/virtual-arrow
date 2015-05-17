{-|
Module: VirtualArrow.Indices
Description: Definitions of indices of proportionality and governability.
-}

module VirtualArrow.Indices
(
    representativeness,
)
where

import VirtualArrow.Input
import VirtualArrow.Election (oneDistrictProportionality)



-- | Calculates <http://en.wikipedia.org/wiki/Gallagher_Index Gallagher Index>
representativeness :: Input -> Parliament -> Double
representativeness input parliament =
    sqrt (1.0/2.0 * fromIntegral s) / 
    fromIntegral (parliamentSize input) * 100.0
  where
    s :: Int
    s = sum $ zipWith
        (\(_, s1) (_, s2) -> (s1 - s2) * (s1 - s2))
        (oneDistrictProportionality input)
        parliament

-- governability :: Parliament -> [(Party, Int)] -> Double
-- governability parliament coalitions =
--     1.0/(m + 1.0) + (1.0/m - 1.0/(m + 1.0))*(f - t/2.0)/t/2.0
--   where
--     numberOfSeats = sum (map snd parliament)
--     coalitionMap = Map.fromList coalitions
--     parliamentMap = Map.fromList parliament
--     majorityCoalition =
--         maximumBy (sum $ map (parliamentMap Map.!))
--             (map 
--                 (\list@(p:_) -> coalitionMap Map.! p, list)
--                 (groupBy snd coalitions)
--             )
--     numberOfMajorParties =
--         if coalitionSeats * 2 > numberOfSeats
--             then length (filter (\p -> parliamentMap Map.! p * 2 > coalitionSeats))
--             else 0
--     coalitionSeats = sum $ map (parliamentMap Map.!) majorityCoalition
--     m = fromIntegral numberOfMajorParties
--     f = fromIntegral coalitionSeats
--     t = fromIntegral numberOfSeats