module VirtualArrow.Input
(
    Preferences,
    Parliament,
    DistrictID,
    VoterID,
    District(..),
    Voter(..),
    Input(..),
    Party,
    NumberOfSeats,
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
    calculateProportion,
    prefToPlaces
) where

import qualified Data.Vector as V
import Data.List (find)
import Data.Maybe (fromMaybe)
import VirtualArrow.Utils ((/.))


{- Data types -}

-- List of parties ordered by voter's preference
type Preferences = V.Vector Int
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


prefToPlaces :: Preferences -> V.Vector Int
prefToPlaces preferences =
    V.map index (V.fromList [0..nparties - 1])
  where
    nparties = length preferences
    index :: Int -> Int
    index x =
        fromMaybe
            (error ("Preferences " ++ show preferences ++ " are not complete."))
            (x `V.elemIndex` preferences)

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
firstChoices input = map (V.head . preferences) (voters input)

firstChoicesAmongVoters :: [Voter] -> [Party]
firstChoicesAmongVoters = map (V.head . preferences)

parliamentSize :: Input -> NumberOfSeats
parliamentSize input = sum (listOfNumberOfSeats input)

nvoters :: Input -> Int
nvoters input = length (voters input)

calculateProportion :: Input -> Int -> Int
calculateProportion input x =
    round ((x * parliamentSize input) /. nvoters input)
