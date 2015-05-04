module Input
(
    Preferences(..),
    Probabilities(..),
    Parliament(..),
    District(..),
    Voter(..),
    Party(..),
    Input(..),
    getListOfNumberOfSeats,
    getVotersByDistrictID,
    numberOfSeats,
    numberOfVoters,
    getFirstChoices,
    calculateProportion
) where
{- Data types -}

-- List of parties ordered by voter's preference
type Preferences = [Int]
-- List of probabilities that voter will choose candidate from a party.
type Probabilities = [Float]

type Parliament = [(Int, Int)]

data District = District
    { district_id :: !Int
    , number_of_seats :: !Int
    }
    deriving (Show)

data Voter = Voter
    { voter_id :: !Int
    , district :: !Int
    , preferences :: Preferences
    , probabilities :: Probabilities
    }
    deriving (Show)

data Party = Party
    { party_id :: !Int
    }
    deriving (Show)

data Input = Input 
    { districts :: [District]
    , voters :: [Voter]
    }


getListOfNumberOfSeats :: Input -> [Int]
getListOfNumberOfSeats input = map (number_of_seats) (districts input)

getVotersByDistrictID :: Input -> Int -> [Voter]
getVotersByDistrictID input district_id = filter (\x -> district x == district_id) (voters input)

getFirstChoices :: Input -> [Int]
getFirstChoices input = map (\voter -> (preferences voter) !! 0) (voters input)

numberOfSeats :: Input -> Int
numberOfSeats input = sum (getListOfNumberOfSeats input)

numberOfVoters :: Input -> Int
numberOfVoters input = length (voters input)

calculateProportion :: Input -> Int -> Int
calculateProportion input x =  
    round $ ((fromIntegral x) * (fromIntegral (numberOfSeats input)) / (fromIntegral $ numberOfVoters input))
