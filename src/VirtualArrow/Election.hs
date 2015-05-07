module VirtualArrow.Election
(
    bordaCount,
    oneDistrictProportionality,
    plurality,
    runOffPlurality,
    multiDistrictProportionality,
    mixedMember1,
    mixedMember2,
    thresholdProportionality,
    singleTransferableVote
) where

import Data.List (elemIndex, groupBy, maximumBy, sortBy, sort, find)
import qualified VirtualArrow.Input as I
import qualified VirtualArrow.Utils as U
import Data.Maybe (fromMaybe)
import Data.Function (on)
import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map


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
            (map snd (I.numberOfSeatsByDistrict input))
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
            (map snd (I.numberOfSeatsByDistrict input))
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

mixedMember1 :: I.Input -> Double -> I.Parliament
mixedMember1 input weight =
    weightedParliament 
        (sort $ plurality input)
        (sort $ multiDistrictProportionality input)
    where
        weightedParliament :: I.Parliament -> I.Parliament -> I.Parliament
        weightedParliament parliament1 parliament2 =
            map (\((party1, seats1), (_, seats2)) ->
                    (party1, wm (fromIntegral seats1) (fromIntegral seats2))
                ) (zip parliament1 parliament2)
            where
                wm :: Double -> Double -> Int
                wm v1 v2 = round
                    ((v1 * weight + v2) / (weight + 1.0))

mixedMember2 :: I.Input -> Double -> I.Parliament
mixedMember2 input share =
    mergeParliaments 
        (plurality I.Input{I.districts=ds1, I.voters=voters, I.numOfParties=numOfParties}) 
        (multiDistrictProportionality I.Input{I.districts=ds2, I.voters=voters, I.numOfParties=numOfParties})
    where
        (ds1, ds2) =
            unzip
                (map 
                    (\(dID, seats) -> 
                        let pluralitySeats = round $ fromIntegral seats * share
                        in
                        (I.District{I.districtID = dID, I.seats = pluralitySeats}, I.District{I.districtID = dID, I.seats=seats - pluralitySeats})
                    )
                    (I.numberOfSeatsByDistrict input)
                )
        numOfParties = I.numOfParties input
        voters = I.voters input
        mergeParliaments :: I.Parliament -> I.Parliament -> I.Parliament
        mergeParliaments = zipWith (\ (party1, s1) (_, s2) -> (party1, s1 + s2))

thresholdProportionality :: I.Input -> Double -> I.Parliament
thresholdProportionality input threshold =
    calculateSeats
        (foldl
            (\acc (party, votes) ->
                if fromIntegral votes / fromIntegral (I.numberOfVoters input) < threshold then
                    acc
                else
                    (fst acc + votes, (party, votes):snd acc)
            )
            (0, [])
            (U.frequences $ I.firstChoices input)
        )
    where
        calculateSeats :: (Int, [(Int, Int)]) -> I.Parliament
        calculateSeats (countedVotes, results) =
            map (\(party, votes) -> 
                    (party, round ((fromIntegral votes :: Double) / (fromIntegral countedVotes :: Double) * fromIntegral (I.numberOfSeats input)))
                )
            results

stv :: M.Matrix Double -> [Int]-> Int -> Int -> [Int]
stv _ winners 0 _ = winners
stv table winners numberOfSeats numOfCandidates =
    if numberOfSeats >=1 && numOfCandidates == 1 then 
        fromMaybe 0 (find (\i -> 0 /= M.getRow i table V.! 0) [1..M.nrows table]) - 1:winners
    else
    case winner of
        Just w ->
            stv (redistribute w) (w - 1:winners) (numberOfSeats - 1) (numOfCandidates - 1)
        Nothing ->
            stv (remove weakest) winners numberOfSeats (numOfCandidates - 1)
    where
        weakest :: Int
        weakest =
            1 + U.minIndex (map (\i -> length (V.filter (<2.0) (M.getRow i table))) [1..M.nrows table])
        remove :: Int -> M.Matrix Double
        remove row =
            M.transpose (M.fromLists
                (map (\i -> V.toList (redistributeColumn (M.getCol i table) (row - 1) 1)) [1..numberOfVoters])
            )
        winner :: Maybe Int
        winner =
            find (\i -> votes i >= quota) [1..M.nrows table]
        quota :: Double
        quota =
            fromIntegral ((floor $ (fromIntegral numberOfVoters :: Double) / ((fromIntegral numberOfSeats :: Double) + 1.0) + 1.0) :: Integer)
        votes :: Int -> Double
        votes i = sum (V.filter (<=1.0)  (M.getRow i table))
        numberOfVoters :: Int
        numberOfVoters = M.ncols table
        redistribute :: Int -> M.Matrix Double
        redistribute w =
            let surplusUnit = (votes w - quota) / votes w
            in
            M.transpose (M.fromLists
                (map (\i -> V.toList (redistributeColumn (M.getCol i table) (w - 1) surplusUnit)) [1..numberOfVoters])
            )
        redistributeColumn :: V.Vector Double -> Int -> Double -> V.Vector Double
        redistributeColumn column w weight =
            if column V.! w <= 1 then
                V.imap
                    (\i x -> if i == w then 0 else
                        if x > 2 then x - 1
                        else
                            if x == 2 then column V.! w * weight 
                            else x
                    )
                    column
            else
                column V.// [(w, 0.0)]

singleTransferableVote :: I.Input -> Map.Map Int Int -> I.Parliament
singleTransferableVote input candidates'Party =
    map 
        (\g -> (fst (head g), sum (map snd g)))
        (groupBy ((==) `on` fst)
            (concatMap
                (\(dID, voters) ->
                    U.frequences
                        (map
                            (\candidate -> candidates'Party Map.! candidate)
                            (stv
                                (table voters)
                                []
                                (I.numberOfSeatsByDistrictID input dID)
                                (I.numOfParties input)
                            )
                        )
                )
                (I.votersByDistrict input)
            )
        )
    where
        table vs =
            M.fromLists (map (\i -> map (\v -> 1 + fromIntegral ( fromMaybe 0 ( elemIndex i (I.preferences v)))) vs) [0..I.numOfParties input - 1])
