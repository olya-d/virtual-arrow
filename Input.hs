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
    { districtID :: !Int
    , numberOfSeats :: !Int
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
    }


getListOfNumberOfSeats :: Input -> [Int]
getListOfNumberOfSeats input = map numberOfSeats (districts input)

getVotersByDistrictID :: Input -> Int -> [Voter]
getVotersByDistrictID input districtID = filter (\x -> district x == districtID) (voters input)

getFirstChoices :: Input -> [Int]
getFirstChoices input = map (head. preferences) (voters input)

seats :: Input -> Int
seats input = sum (getListOfNumberOfSeats input)

numberOfVoters :: Input -> Int
numberOfVoters input = length (voters input)

calculateProportion :: Input -> Int -> Int
calculateProportion input x =  
    round (fromIntegral x * fromIntegral (seats input) / fromIntegral (numberOfVoters input))
