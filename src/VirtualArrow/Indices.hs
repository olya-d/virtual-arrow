{-|
Module: VirtualArrow.Indices
Description: Definitions of indices of proportionality and governability.
-}

module VirtualArrow.Indices
(
    gallagherIndex,
    governability
)
where

import VirtualArrow.Input
import VirtualArrow.Election (oneDistrictProportionality)
import qualified Data.Map.Strict as Map
import Control.Arrow
import Data.List (groupBy, sortBy)
import Data.Function (on)


-- | Computes <http://en.wikipedia.org/wiki/Gallagher_Index Gallagher Index>
gallagherIndex :: Input -> Parliament -> Maybe (Map.Map Int Party)-> Double
gallagherIndex input parliament candidates =
    sqrt (1.0/2.0 * fromIntegral s) / 
    fromIntegral (parliamentSize input) * 100.0
  where
    s :: Int
    s = sum $ zipWith
        (\(_, s1) (_, s2) -> (s1 - s2) * (s1 - s2))
        proportionalityParliament
        parliament
    proportionalityParliament :: Parliament
    proportionalityParliament =
        case candidates of
            Just m -> 
                map 
                    (first ((Map.!) m))
                    (oneDistrictProportionality input)
            Nothing -> oneDistrictProportionality input


-- | Computes index of governability as described in 
-- <http://jasss.soc.surrey.ac.uk/7/3/3.html#1.22>
governability :: Parliament 
              -> [(Party, Int)] -- ^ assignment of party to coalition
              -> Double
governability parliament coalitions =
    1.0/(m + 1.0) + (1.0/m - 1.0/(m + 1.0))*(f - t/2.0)/(t/2.0)
  where
    numberOfSeats :: Int
    numberOfSeats = sum (map snd parliament)
    coalitionMap :: Map.Map Party Int
    coalitionMap = Map.fromList coalitions
    parliamentMap :: Map.Map Party NumberOfSeats
    parliamentMap = Map.fromList parliament
    (majorityCoalitionSeats, majorityCoalition) =
        maximum $ zip
            (map
                numberOfSeatsInCoalition
                (groupBy ((==) `on` snd) (sortBy (compare `on` snd) coalitions))
            )
            [0..]
    numberOfSeatsInCoalition :: [(Party, Int)] -> Int
    numberOfSeatsInCoalition coalition =
        sum $ map (\(p, _) -> parliamentMap Map.! p) coalition
    numberOfMajorParties :: Int
    numberOfMajorParties =
        if majorityCoalitionSeats * 2 > numberOfSeats then
            length (filter destroysMajority parliament)
        else 0
    destroysMajority :: (Party, NumberOfSeats) -> Bool
    destroysMajority (p, seats) =
        coalitionMap Map.! p == majorityCoalition && 
        2*(majorityCoalitionSeats - seats) < numberOfSeats
    m = fromIntegral numberOfMajorParties
    f = fromIntegral majorityCoalitionSeats
    t = fromIntegral numberOfSeats
