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


bordaCount :: I.Input -> I.Parliament
bordaCount input = 
    map
        (\g -> (snd (head g), sum (map fst g))) 
        (groupBy 
            ((==) `on` snd) 
            (zip 
                (I.numberOfSeatsByDistrict input) 
                (map (U.minIndex . \vs -> bordaCountAmongVoters vs pn) (I.votersByDistrict input))
            )
        )
    where
        pn = I.numOfParties input
        bordaCountAmongVoters :: [I.Voter] -> Int -> [Int]
        bordaCountAmongVoters voters numOfParties =
            foldl accVotes (replicate numOfParties 0) voters
        accVotes :: [Int] -> I.Voter -> [Int]
        accVotes acc voter =
            [(acc !! i) + getVote (I.preferences voter) i | i <- [0..length acc - 1]]
            where
                getVote preferences partyID = 
                    fromMaybe (error (show partyID ++ show preferences)) (elemIndex partyID preferences) 


oneDistrictProportionality :: I.Input -> I.Parliament
oneDistrictProportionality input = 
    map (\x -> (fst x, I.calculateProportion input (snd x))) (U.frequences $ I.getFirstChoices input)
