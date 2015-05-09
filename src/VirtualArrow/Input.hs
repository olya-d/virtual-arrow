module VirtualArrow.Input
(
    Preferences,
    Parliament,
    DistrictID,
    District(..),
    Voter(..),
    Input(..),
    Party,
    NumberOfSeats,
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
import Data.Maybe (fromMaybe)
import VirtualArrow.Utils ((/.))


-- List of parties ordered by voter's preference
type Preferences = V.Vector Int
type Parliament = [(Int, Int)]

type DistrictID = Int
type NumberOfSeats = Int
type Party = Int

data District = District
    { districtID :: !DistrictID
    , nseats :: !NumberOfSeats
    }
    deriving (Show)

data Voter = Voter
    { voterID :: !Int
    , district :: !DistrictID
    , preferences :: Preferences
    }
    deriving (Show)

data Input = Input 
    { districts :: [District]
    , voters :: [Voter]
    , nparties :: !Int
    }
    deriving (Show)


-- | Returns vector of places,
-- | s.t. ! i = place of ith party in the list of preferences.
prefToPlaces :: Preferences -> V.Vector Int
prefToPlaces pref =
    V.map index (V.fromList [0..p - 1])
  where
    p = length pref 
    index :: Int -> Int
    index x =
        fromMaybe
            (error ("Preferences " ++ show pref ++ " are not complete."))
            (x `V.elemIndex` pref)

-- | Returns list of district ids.
districtIDs :: Input -> [DistrictID]
districtIDs input = map districtID (districts input)

-- | Returns list of voters by district id.
votersByDistrictID :: Input -> DistrictID -> [Voter]
votersByDistrictID input dId = filter ((== dId) . district) (voters input)

-- | Returns list of pairs (district id, voters in the district).
votersByDistrict :: Input -> [(DistrictID, [Voter])]
votersByDistrict input =
    [(i, votersByDistrictID input i) | i <- districtIDs input]

-- | Returns number of seats by district id.
numberOfSeatsByDistrictID :: Input -> DistrictID -> NumberOfSeats
numberOfSeatsByDistrictID input dID = 
    nseats (head $ filter (\x -> districtID x == dID) (districts input))

-- | Returns list of pairs (district id, number of seats in the district).
numberOfSeatsByDistrict :: Input -> [(DistrictID, NumberOfSeats)]
numberOfSeatsByDistrict input = 
    [(i, numberOfSeatsByDistrictID input i) | i <- districtIDs input]

-- | Returns list of first choices (first preference) 
-- | for each voter in the input.
firstChoices :: Input -> [Party]
firstChoices input = map (V.head . preferences) (voters input)

-- | Returns list of first choices (first preference) for each voter
-- | in the list.
firstChoicesAmongVoters :: [Voter] -> [Party]
firstChoicesAmongVoters = map (V.head . preferences)

-- | = total number of seats.
parliamentSize :: Input -> NumberOfSeats
parliamentSize input = sum (map nseats (districts input))

-- | = total number of voters.
nvoters :: Input -> Int
nvoters input = length (voters input)

-- | Returns proportion of parliament corresponding to x (number of votes). 
calculateProportion :: Input -> Int -> Int
calculateProportion input x =
    round ((x * parliamentSize input) /. nvoters input)
