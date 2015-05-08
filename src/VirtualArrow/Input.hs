module VirtualArrow.Input
(
    Preferences,
    Parliament,
    District(..),
    Voter(..),
    Input(..),
    listOfNumberOfSeats,
    voterByID,
    votersByDistrictID,
    votersByDistrict,
    parliamentSize,
    numberOfSeatsByDistrictID,
    numberOfSeatsByDistrict,
    nvoters,
    firstChoices,
    firstChoicesAmongVoters,
    calculateProportion
) where

import Data.List (find)
import Data.Maybe (fromMaybe)
import VirtualArrow.Utils ((/.))


{- Data types -}

-- List of parties ordered by voter's preference
type Preferences = [Int]
type Parliament = [(Int, Int)]

type DistrictID = Int
type VoterID = Int
type NumberOfSeats = Int
type NumberOfParties = Int
type Party = Int

data District = District
    { districtID :: !DistrictID
    , nseats :: !NumberOfSeats
    }
    deriving (Show)

data Voter = Voter
    { voterID :: !VoterID
    , district :: !DistrictID
    , preferences :: Preferences
    }
    deriving (Show)

data Input = Input 
    { districts :: [District]
    , voters :: [Voter]
    , nparties :: !NumberOfParties
    }
    deriving (Show)


-- Private, returns the list of numbers of seats in each district.
listOfNumberOfSeats :: Input -> [Int]
listOfNumberOfSeats input = map nseats (districts input)

-- Get voter by id.
voterByID :: Input -> VoterID -> Voter
voterByID input vId =
    fromMaybe
        (error ("Voter with id " ++ show vId ++ " is not found."))
        (find ((== vId) . voterID) (voters input))

-- Get list of voters by district id.
votersByDistrictID :: Input -> DistrictID -> [Voter]
votersByDistrictID input dId = filter ((== dId) . district) (voters input)

votersByDistrict :: Input -> [(DistrictID, [Voter])]
votersByDistrict input = [(i, votersByDistrictID input i) | i <- map districtID (districts input)]

numberOfSeatsByDistrictID :: Input -> DistrictID -> NumberOfSeats
numberOfSeatsByDistrictID input dID = nseats (head $ filter (\x -> districtID x == dID) (districts input))

numberOfSeatsByDistrict :: Input -> [(DistrictID, NumberOfSeats)]
numberOfSeatsByDistrict input = 
    [(i, numberOfSeatsByDistrictID input i) | i <- map districtID (districts input)]

firstChoices :: Input -> [Party]
firstChoices input = map (head. preferences) (voters input)

firstChoicesAmongVoters :: [Voter] -> [Party]
firstChoicesAmongVoters = map (head. preferences)

parliamentSize :: Input -> NumberOfSeats
parliamentSize input = sum (listOfNumberOfSeats input)

nvoters :: Input -> Int
nvoters input = length (voters input)

calculateProportion :: Input -> Int -> Int
calculateProportion input x =
    round ((x * parliamentSize input) /. nvoters input)
