module VirtualArrow.Input
(
    Preferences,
    Probabilities,
    Parliament,
    District(..),
    Voter(..),
    Party(..),
    Input(..),
    listOfNumberOfSeats,
    voterByID,
    votersByDistrictID,
    votersByDistrict,
    numberOfSeats,
    numberOfSeatsByDistrictID,
    numberOfSeatsByDistrict,
    numberOfVoters,
    firstChoices,
    firstChoicesAmongVoters,
    calculateProportion
) where

import VirtualArrow.Utils ((/.))
{- Data types -}

-- List of parties ordered by voter's preference
type Preferences = [Int]
-- List of probabilities that voter will choose candidate from a party.
type Probabilities = [Float]

type Parliament = [(Int, Int)]

data District = District
    { districtID :: !Int
    , seats :: !Int
    }
    deriving (Show)

data Voter = Voter
    { voterID :: !Int
    , district :: !Int
    , preferences :: Preferences
    , probabilities :: Probabilities
    }
    deriving (Show)

data Party = Party
    { partyID :: !Int
    }
    deriving (Show)

data Input = Input 
    { districts :: [District]
    , voters :: [Voter]
    , numOfParties :: !Int
    }


listOfNumberOfSeats :: Input -> [Int]
listOfNumberOfSeats input = map seats (districts input)

voterByID :: Input -> Int -> Voter
voterByID input vID = head (filter (\v -> voterID v == vID) (voters input))

votersByDistrictID :: Input -> Int -> [Voter]
votersByDistrictID input dID = filter (\x -> district x == dID) (voters input)

votersByDistrict :: Input -> [(Int, [Voter])]
votersByDistrict input = [(i, votersByDistrictID input i) | i <- map districtID (districts input)]

numberOfSeatsByDistrictID :: Input -> Int -> Int
numberOfSeatsByDistrictID input dID = seats (head $ filter (\x -> districtID x == dID) (districts input))

numberOfSeatsByDistrict :: Input -> [(Int, Int)]
numberOfSeatsByDistrict input = 
    [(i, numberOfSeatsByDistrictID input i) | i <- map districtID (districts input)]

firstChoices :: Input -> [Int]
firstChoices input = map (head. preferences) (voters input)

firstChoicesAmongVoters :: [Voter] -> [Int]
firstChoicesAmongVoters = map (head. preferences)

numberOfSeats :: Input -> Int
numberOfSeats input = sum (listOfNumberOfSeats input)

numberOfVoters :: Input -> Int
numberOfVoters input = length (voters input)

calculateProportion :: Input -> Int -> Int
calculateProportion input x =  
    round ((x * numberOfSeats input) /. numberOfVoters input)
