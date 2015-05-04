module Election
(
    bordaCount
) where

import Data.List (elemIndex, groupBy)

accVotes :: [Int] -> ([Int], [Float]) -> [Int]
accVotes acc voter =
    [(acc !! i) + getVote (fst voter) i | i <- [0 .. (length acc) - 1]]
    where
        getVote preferences party_id = case (elemIndex party_id preferences) of
            Just n -> n
            Nothing -> error ((show party_id) ++ (show preferences))

bordaCountInDistrict :: [([Int], [Float])] -> Int -> [Int]
bordaCountInDistrict voters numOfParties =
    foldl (\acc v -> accVotes acc v) (replicate numOfParties 0) voters

minIndex :: (Ord a) => [a] -> Int
minIndex list = snd . minimum $ zip list [0 .. ]

listOfResultsByDistrict :: [([([Int], [Float])], Int)] -> Int -> [Int]
listOfResultsByDistrict input numOfParties = 
    map minIndex (map (\d -> bordaCountInDistrict (fst d) numOfParties) input)

bordaCount :: [([([Int], [Float])], Int)] -> Int -> [(Int, Int)]
bordaCount input numOfParties = 
    map (\g -> (sum (map (fst) g), snd (g !! 0))) (groupBy (\x y -> snd x == snd y) (zip (map (snd) input) (listOfResultsByDistrict input numOfParties)))
