{-|
Module: VirtualArrow.Input
Description: Definitions of data structures.

This module defines data structures and accompanying functions.
-}

module VirtualArrow.Input
(
    -- * Types
    Preferences,
    DistrictID,
    Party,
    NumberOfSeats,
    Parliament,
    -- * Data types
    Candidate(..),
    District(..),
    Voter(..),
    Input(..),
    -- * Functions
    parliamentSize,
    numberOfSeatsByDistrictID,
    numberOfSeatsByDistrict,
    nvoters,
    firstChoices,
    firstChoicesAmongVoters,
    calculateProportion,
    prefToPlaces,
    votersByDistrict,
    candidateMap
) where

import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import VirtualArrow.Utils ((/.))
import Data.List (groupBy)
import Data.Function (on)
import Control.Arrow ((&&&))


-- | Vector of parties ordered by voter's preference.
type Preferences = V.Vector Int

type DistrictID = Int
type NumberOfSeats = Int
type Party = Int

-- | The resulting parliament.
type Parliament = [(Party, NumberOfSeats)]

data Candidate = Candidate
    { candidateID :: !Int
    , party :: !Int
    }
    deriving (Show)

-- | Represents one electoral district (constituency).
data District = District
    { districtID :: !DistrictID
    , nseats :: !NumberOfSeats
    }
    deriving (Show)

-- | Represents one voter in a district. 
data Voter = Voter
    { district :: !DistrictID
    , preferences :: !Preferences
    }
    deriving (Show)

-- | Represents input to the program - collection of districts, voters
-- number of parties and internally calculated (using 'votersByDistrict') 
-- map from district id to votes.
data Input = Input
    { districts :: ![District]
    , voters :: ![Voter]
    , nparties :: !Int
    , districtMap :: Map.Map DistrictID [Voter]
    }
    deriving (Show)


-- | Returns vector of places,
-- s.t. @! i@ is the place of ith party in the list of preferences.
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

-- | Returns number of seats by district id.
numberOfSeatsByDistrictID :: Input -> DistrictID -> NumberOfSeats
numberOfSeatsByDistrictID input dID =
    nseats (head $ filter (\x -> districtID x == dID) (districts input))

numberOfSeatsByDistrict :: Input -> [(DistrictID, NumberOfSeats)]
numberOfSeatsByDistrict input =
    [(i, numberOfSeatsByDistrictID input i) | i <- districtIDs input]

-- | Returns list of first choices (first preference)
-- for each voter in the input.
firstChoices :: Input -> [Party]
firstChoices input = map (V.head . preferences) (voters input)

-- | Returns list of first choices (first preference) for each voter
-- in the list.
firstChoicesAmongVoters :: [Voter] -> [Party]
firstChoicesAmongVoters = map (V.head . preferences)

-- | Total number of seats.
parliamentSize :: Input -> NumberOfSeats
parliamentSize input = sum (map nseats (districts input))

-- | Total number of voters.
nvoters :: Input -> Int
nvoters input = length (voters input)

-- | Returns proportion of parliament corresponding to the number of votes.
calculateProportion :: Input 
                  -> Int -- ^ number of votes
                  -> Int
calculateProportion input x =
    round ((x * parliamentSize input) /. nvoters input)

votersByDistrict :: [Voter] -> [(DistrictID, [Voter])]
votersByDistrict vs =
    map (\g -> (district (head g), g)) (groupBy ((==) `on` district) vs)

-- | Returns map from candidate id to party.
candidateMap :: [Candidate] -> Map.Map Int Party
candidateMap list = Map.fromList $ 
    map (candidateID Control.Arrow.&&& party) list
