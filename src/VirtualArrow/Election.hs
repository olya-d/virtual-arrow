{-|
Module: VirtualArrow.Election
Description: Functions that implement election systems.
-}

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

import Data.List (groupBy, maximumBy, sortBy, sort, find, findIndex)
import qualified VirtualArrow.Input as I
import qualified VirtualArrow.Utils as U
import Data.Maybe (fromMaybe)
import Data.Function (on)
import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Control.Arrow ((***), second)

type PartyResult = (I.NumberOfSeats, I.Party)

sumSeatsAcrossDistricts :: [PartyResult] -> I.Parliament
sumSeatsAcrossDistricts results =
    map
        (\g -> (snd (head g), sum (map fst g))) 
        (groupBy 
            ((==) `on` snd) 
            (sortBy (compare `on` snd) results)
        )

-- | Each party gets a number of points for each ballot:
-- 0 if it is on the first place in the ballot, 1 if on the second
-- and so on till n - 1 points. A candidate with the least number of votes wins.
-- To determine the resulting parliament find a winner for each district. 
-- Assign all seats to the winner, sum seats across districts.
bordaCount :: I.Input -> I.Parliament
bordaCount input =
    sumSeatsAcrossDistricts $
        map
            (I.numberOfSeatsByDistrictID input *** winner)
            (Map.toList $ I.districtMap input) -- [(I.DistrictID, [I.Voter])]
  where
    winner :: [I.Voter] -> I.Party
    winner [] = error "Can't find a winner without voters."
    winner voters =
        V.minIndex (foldl accVotes (V.replicate nparties 0) voters)
      where
        nparties :: Int
        nparties = length $ I.preferences (head voters)
        accVotes :: V.Vector Int -> I.Voter -> V.Vector Int
        accVotes acc voter =
            V.zipWith (+) acc (I.prefToPlaces (I.preferences voter))


-- | Districts are aggregated into one and seats in the parliament are 
-- distributed proportionally to the first preferences in the electorate.
oneDistrictProportionality :: I.Input -> I.Parliament
oneDistrictProportionality input =
    map
        (second (I.calculateProportion input))
        (U.frequencies $ I.firstChoices input)

-- | The party with the most votes wins in each district.
plurality :: I.Input -> I.Parliament
plurality input =
    sumSeatsAcrossDistricts $
        map 
            (I.numberOfSeatsByDistrictID input *** winner)
            (Map.toList $ I.districtMap input)
  where
    winner :: [I.Voter] -> I.Party
    winner voters =
        fst $ 
            maximumBy
                (compare `on` snd)
                (U.frequencies (I.firstChoicesAmongVoters voters))


-- | In each district all parties but the two with the most votes are excluded.
-- The second round is implemented with these two parties only and 
-- the one with the most votes wins. 
-- If after the first round the first party has at least 50% of the votes, 
-- it is elected without the need of a second round. 
runOffPlurality :: I.Input -> I.Parliament
runOffPlurality input =
    sumSeatsAcrossDistricts $
        map
            (I.numberOfSeatsByDistrictID input *** winner)
            (Map.toList $ I.districtMap input)
  where
    winner :: [I.Voter] -> I.Party
    winner voters =
         -- TODO: when both parties have 50% (run-off is meaningless)
        case top2 of
            [(party, _)] -> party
            [(p1, v1), (p2, _)] ->
                if hasMajority v1 then p1 else runOff p1 p2
      where
        top2 :: [(I.Party, Int)]
        top2 =
            take 2 $
                sortBy
                    (flip compare `on` snd)
                    (U.frequencies $ I.firstChoicesAmongVoters voters)
        hasMajority :: Int -> Bool
        hasMajority votes = length voters <= (votes * 2)
        runOff :: I.Party -> I.Party -> I.Party
        runOff p1 p2
            | 2 * U.count (prefer1 . I.preferences) voters > length voters = p1
            | otherwise                                                    = p2
          where
            prefer1 preferences =
                V.elemIndex p1 preferences > V.elemIndex p2 preferences

-- | Helper function for multiDistrictProportionality.
-- [(I.Party, number of votes, number of allocated seats)] --> seats left ->
-- [(I.Party, number of allocated seats)]
distributeSeats :: V.Vector (I.Party, Int, I.NumberOfSeats) -> Int-> [PartyResult]
distributeSeats result 0 = V.toList $ V.map (\(p, _, s) -> (s, p)) result
distributeSeats result seatsLeft =
    distributeSeats 
        (result V.// [(maxQuotaIndex, (party, votes, currSeats + 1))])
        (seatsLeft - 1)
  where
    (party, votes, currSeats) = result V.! maxQuotaIndex
    maxQuotaIndex :: Int
    maxQuotaIndex = V.maxIndex (V.map quota result)
    quota :: (I.Party, Int, Int) -> Double
    quota (_, v, s) = v U./. (s * 2 + 1)

-- | In each district seats are allocated using 
-- <http://en.wikipedia.org/wiki/Sainte-Lagu%C3%AB_method Sainte-Laguë method>:
-- successive quotients are calculated for each party, whichever party has
-- the highest quotient gets the next seat allocated.
-- The process is repeated until all seats have been allocated.
multiDistrictProportionality :: I.Input -> I.Parliament
multiDistrictProportionality input =
    sumSeatsAcrossDistricts $
        -- concatMap is used, since district does not have a single winner
        concatMap sainteLague (Map.toList $ I.districtMap input)
  where
    sainteLague :: (I.DistrictID, [I.Voter]) -> [PartyResult]
    sainteLague (districtID, voters) =
        distributeSeats
            (V.fromList $
                map 
                    (\(party, votes) -> (party, votes, 0))
                    (U.frequencies (I.firstChoicesAmongVoters voters))
            )
            (I.numberOfSeatsByDistrictID input districtID)

{--------------------------------------------------------------------
  Mixed Member System, 1 
--------------------------------------------------------------------} 

-- | One parliament is elected with the Plurality System,
-- and one with Proportional System.
-- The resulting parliament is a weighted mean of the two.
-- Weight of the first parliament relative to the second is specified.

mixedMember1 :: I.Input -> Double -> I.Parliament
mixedMember1 input weight =
    weightedParliament 
        (sort $ plurality input)
        (sort $ multiDistrictProportionality input)
  where
    weightedParliament :: I.Parliament -> I.Parliament -> I.Parliament
    weightedParliament =
        zipWith
            (\(party1, seats1) (_, seats2) ->
                (party1, mean (fromIntegral seats1) (fromIntegral seats2))
            )
      where
        mean :: Double -> Double -> Int
        mean v1 v2 = round (v1 * weight + (1 - weight) * v2)

{--------------------------------------------------------------------
  Mixed Member System, 2
--------------------------------------------------------------------} 

-- | Part of parliament is elected with the Plurality System,
-- and the remainder is elected using the Proportional System.
-- Share of seats to be elected with the Plurality System is specified.

mixedMember2 :: I.Input -> Double -> I.Parliament
mixedMember2 input share =
    mergeParliaments 
        (plurality 
            I.Input
            { I.districts=pluralityDistricts
            , I.voters=I.voters input
            , I.nparties=I.nparties input
            , I.districtMap=Map.fromList $ I.votersByDistrict (I.voters input)
            }
        ) 
        (multiDistrictProportionality
            I.Input
            { I.districts=proportionalityDistricts
            , I.voters=I.voters input
            , I.nparties=I.nparties input
            , I.districtMap=Map.fromList $ I.votersByDistrict (I.voters input)
            }
        )
    where
        (pluralityDistricts, proportionalityDistricts) =
            unzip $ map twoNewDistricts (I.districts input)
        twoNewDistricts :: I.District -> (I.District, I.District)
        twoNewDistricts district =
            (I.District
            { I.districtID=I.districtID district
            , I.nseats=pluralitySeats}, 
            I.District
            { I.districtID=I.districtID district
            , I.nseats=I.nseats district - pluralitySeats})
          where
            pluralitySeats = round $ fromIntegral (I.nseats district) * share
        mergeParliaments :: I.Parliament -> I.Parliament -> I.Parliament
        mergeParliaments = zipWith (\(p, s1) (_, s2) -> (p, s1 + s2))



-- | All the parties who have an overall share of votes (strictly) smaller  
-- are excluded from the parliament. 
-- The seats are distributed proportionally among the remaining parties. 
-- There is only one district.
thresholdProportionality :: I.Input -> Double -> I.Parliament
thresholdProportionality input threshold =
    calculateSeats $
        filter ((>= requiredVotes) . snd) (U.frequencies $ I.firstChoices input)
  where
    requiredVotes :: Int
    requiredVotes = round $ threshold * fromIntegral (I.nvoters input)
    calculateSeats :: [(Int, I.Party)] -> I.Parliament
    calculateSeats results =
        map (second seats) results
      where
        parliamentSizeToNumberOfCountedVotes :: Double
        parliamentSizeToNumberOfCountedVotes =
            fromIntegral (I.parliamentSize input) /
            fromIntegral (sum $ map snd results)
        seats :: Int -> Int
        seats votes =
            round (fromIntegral votes * parliamentSizeToNumberOfCountedVotes)


-- | Helper function for singleTransferableVote
-- The current state (table) is a 2d array, s.t. [i,j] element
-- = the order of preference of the ith candidate for the jth voter,
-- starting from 1.
stv :: M.Matrix Double -> [I.Party]-> I.NumberOfSeats -> Int -> [I.Party]
stv _ winners 0 _ = winners
stv table winners numberOfSeats numOfCandidates =
    -- if one seat and one candidate is left, 
    -- find the candidate that was not removed (does not have 0s in the row).
    if numberOfSeats == 1 && numOfCandidates == 1 then 
        fromMaybe 0 
            (findIndex (\r -> 0.0 /= (r V.! 0)) tableRows):winners
    else
    case winner of
        Just w ->
            stv 
                (redistribute w) 
                (w - 1:winners) 
                (numberOfSeats - 1)
                (numOfCandidates - 1)
        Nothing ->
            stv remove winners numberOfSeats (numOfCandidates - 1)
  where
    tableRows = [M.getRow i table | i <- [1..M.nrows table]]
    tableColumns = [M.getCol i table | i <- [1..M.ncols table]]
    weakest :: Int
    weakest = 1 + U.minIndex (map (length . V.filter (<2.0)) tableRows)
    remove :: M.Matrix Double
    remove =
        M.transpose $ M.fromLists $
            map 
                (\v -> V.toList $ redistributeColumn v (weakest - 1) 1) 
                tableColumns
    winner :: Maybe Int
    winner =
        find (\i -> votes i >= quota) [1..M.nrows table]
    quota :: Double
    quota =
        fromIntegral (
            (floor $ (fromIntegral numberOfVoters :: Double) /
            ((fromIntegral numberOfSeats :: Double) + 1.0) + 1.0) :: Integer
        )
    votes :: Int -> Double
    votes i = sum $ V.filter (<=1.0) (M.getRow i table)
    numberOfVoters :: Int
    numberOfVoters = M.ncols table
    redistribute :: Int -> M.Matrix Double
    redistribute w =
        M.transpose $ M.fromLists
            (map 
                (\c -> V.toList (redistributeColumn c (w - 1) surplusUnit)) 
                tableColumns
            )
      where
        surplusUnit = (votes w - quota) / votes w
    -- Matrix is transposed when the candidate is removed,
    -- so a columns is redistributed instead of a row.
    redistributeColumn :: V.Vector Double -> Int -> Double -> V.Vector Double
    redistributeColumn column w weight =
        if column V.! w <= 1 then
            V.imap newValue column
        else
            column V.// [(w, 0.0)]
      where
        newValue index old
            | index == w    = 0  -- This is the row we are removing
            | old > 2       = old - 1 -- Increase preference
            | old == 2      = column V.! w * weight -- Transfer vote
            | otherwise     = old

-- | The seats for each party in each district are assigned 
-- according to a quota value. If some seats are not assigned, 
-- the votes unused by the elected candidates are transferred 
-- to the next candidate in the elector's preference ordering,
-- and the candidates with the highest number of votes are elected. 
-- The operation is repeated until all the seats have been assigned. 
-- If at a given round there is no assignment, 
-- the candidate with less preference is excluded,
-- and its votes are distributed as above. 
-- The Droop quota is used and the votes are redistributed using
-- the Gregory method (transfers partial votes).
-- Single Transferable Vote is used with the list of candidates,
-- thus the additional parameter (map) is required to determine
-- the number of seats for each party.
singleTransferableVote :: I.Input 
                       -> Map.Map Int Int  -- ^ keys are candidate ids and values are their parties
                       -> I.Parliament
singleTransferableVote input candidates'Party =
    U.frequencies $
        concatMap
            (map (candidates'Party Map.!) . stvInDistrict)
            (I.districts input)
  where
    stvInDistrict :: I.District -> [I.Party]
    stvInDistrict district =
        stv (table voters) [] (I.nseats district) (I.nparties input)
      where
        voters :: [I.Voter]
        voters = I.districtMap input Map.! I.districtID district
    table :: [I.Voter] -> M.Matrix Double
    table vs = M.transpose $ M.fromLists (map oneVoterBallot vs)
    oneVoterBallot :: I.Voter -> [Double]
    oneVoterBallot voter = 
        V.toList $ 
            V.map
                ((+1) . fromIntegral) 
                (I.prefToPlaces $ I.preferences voter)
