module VirtualArrow.Election
(
    bordaCount,
    oneDistrictProportionality
) where

import Data.List (elemIndex, groupBy)
import qualified VirtualArrow.Input as I
import qualified VirtualArrow.Utils as U
import Data.Maybe (fromMaybe)
import Data.Function (on)

accVotes :: [Int] -> ([Int], [Float]) -> [Int]
accVotes acc voter =
    [(acc !! i) + getVote (fst voter) i | i <- [0..length acc - 1]]
    where
        getVote preferences party_id = 
            fromMaybe (error (show party_id ++ show preferences)) (elemIndex party_id preferences)

bordaCountInDistrict :: [([Int], [Float])] -> Int -> [Int]
bordaCountInDistrict voters numOfParties =
    foldl accVotes (replicate numOfParties 0) voters

minIndex :: (Ord a) => [a] -> Int
minIndex list = snd . minimum $ zip list [0 .. ]

listOfResultsByDistrict :: [([([Int], [Float])], Int)] -> Int -> [Int]
listOfResultsByDistrict input numOfParties = 
   map (minIndex . (\ d -> bordaCountInDistrict (fst d) numOfParties)) input

bordaCount :: [([([Int], [Float])], Int)] -> Int -> [(Int, Int)]
bordaCount input numOfParties = 
    map (\g -> (sum (map fst g), snd (head g))) (groupBy ((==) `on` snd) (zip (map snd input) (listOfResultsByDistrict input numOfParties)))

oneDistrictProportionality :: I.Input -> [(Int, Int)] 
oneDistrictProportionality input = 
    map (\x -> (fst x, I.calculateProportion input (snd x))) (U.frequences $ I.getFirstChoices input)
