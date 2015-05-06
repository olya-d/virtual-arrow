module VirtualArrow.Election
(
    bordaCount,
    oneDistrictProportionality,
    plurality,
    runOffPlurality,
    multiDistrictProportionality
) where

import Data.List (elemIndex, groupBy, maximumBy, sortBy)
import qualified VirtualArrow.Input as I
import qualified VirtualArrow.Utils as U
import Data.Maybe (fromMaybe)
import Data.Function (on)


sumSeatsAcrossDistricts :: [(Int, Int)] -> I.Parliament
sumSeatsAcrossDistricts results =
    map
        (\g -> (snd (head g), sum (map fst g))) 
        (groupBy 
            ((==) `on` snd) 
            results
        )  

bordaCount :: I.Input -> I.Parliament
bordaCount input =
    sumSeatsAcrossDistricts
        (zip 
            (I.numberOfSeatsByDistrict input) 
            (map (U.minIndex . \x -> bordaCountAmongVoters (snd x) pn) (I.votersByDistrict input))
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
    map
        (\x -> (fst x, I.calculateProportion input (snd x))) 
        (U.frequences $ I.firstChoices input)

plurality :: I.Input -> I.Parliament
plurality input =
    sumSeatsAcrossDistricts
        (zip 
            (I.numberOfSeatsByDistrict input) 
            (map (winner . I.firstChoicesAmongVoters . snd) (I.votersByDistrict input))
        )
    where
        winner choices =
            fst (maximumBy (compare `on` snd) (U.frequences choices))

runOffPlurality :: I.Input -> I.Parliament
runOffPlurality input =
    sumSeatsAcrossDistricts
        (map
            ((\(dID, parties) -> 
                if length parties == 2 then
                    (dID, runOff (dID, parties))
                else
                    (dID, head parties)
            ) . firstTwoOrOne
            )
            (I.votersByDistrict input)
        )
    where
        firstTwoOrOne :: (Int, [I.Voter]) -> (Int, [Int])
        firstTwoOrOne (districtID, voters) =
            if majority (snd (head top)) && not (majority (snd (last top))) then
                (I.numberOfSeatsByDistrictID input districtID, take 1 (map fst top))
            else
                (I.numberOfSeatsByDistrictID input districtID, map fst top)
            where 
                top :: [(Int, Int)]
                top = take 2 (sortBy (flip compare `on` snd) (U.frequences $ I.firstChoicesAmongVoters voters))
                majority :: Int -> Bool
                majority result = result * 2 >= length voters
        runOff :: (Int, [Int]) -> Int
        runOff (districtID, couple) =
            if 2 * length (filter (\p -> elemIndex fP p > elemIndex sP p) (map I.preferences voters)) > length voters then
                fP
            else
                sP
            where
                fP = head couple
                sP = last couple
                voters = I.votersByDistrictID input districtID

distributeSeats :: [(Int, Int, Int)] -> Int-> [(Int, Int, Int)]
distributeSeats result 0 = result
distributeSeats result seatsLeft =
    let (x,(party, votes, currSeats):y) = splitAt (maxQuotaIndex - 1) result
    in
    distributeSeats (x ++ [(party, votes, currSeats + 1)] ++ y) (seatsLeft - 1)
    where
        maxQuotaIndex = 
            U.maxIndex (map quota result)
        quota (_, votes, seats) = 
            votes U./. (seats * 2 + 1)

multiDistrictProportionality :: I.Input -> I.Parliament
multiDistrictProportionality input =
    sumSeatsAcrossDistricts $ concatMap sainteLague (I.votersByDistrict input)
    where
        sainteLague :: (Int, [I.Voter]) -> [(Int, Int)]
        sainteLague (districtID, voters) =
            map
                (\(party, _, seats) -> (seats, party))
                (distributeSeats
                    (map 
                        (\(party, votes) -> (party, votes, 0))
                        (U.frequences (I.firstChoicesAmongVoters voters))
                    )
                    (I.numberOfSeatsByDistrictID input districtID)
                )