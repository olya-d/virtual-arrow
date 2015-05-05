module VirtualArrow.Input
(
    Preferences,
    Probabilities,
    Parliament,
    District(..),
    Voter(..),
    Party(..),
    Input(..),
    getListOfNumberOfSeats,
    getVotersByDistrictID,
    votersByDistrict,
    numberOfSeats,
    numberOfSeatsByDistrictID,
    numberOfSeatsByDistrict,
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


getListOfNumberOfSeats :: Input -> [Int]
getListOfNumberOfSeats input = map seats (districts input)

getVotersByDistrictID :: Input -> Int -> [Voter]
getVotersByDistrictID input dID = filter (\x -> district x == dID) (voters input)

votersByDistrict :: Input -> [[Voter]]
votersByDistrict input = [getVotersByDistrictID input i | i <- map districtID (districts input)]

numberOfSeatsByDistrictID :: Input -> Int -> Int
numberOfSeatsByDistrictID input dID = seats (head $ filter (\x -> districtID x == dID) (districts input))

numberOfSeatsByDistrict :: Input -> [Int]
numberOfSeatsByDistrict input = [numberOfSeatsByDistrictID input i | i <- map districtID (districts input)]

getFirstChoices :: Input -> [Int]
getFirstChoices input = map (head. preferences) (voters input)

numberOfSeats :: Input -> Int
numberOfSeats input = sum (getListOfNumberOfSeats input)

numberOfVoters :: Input -> Int
numberOfVoters input = length (voters input)

calculateProportion :: Input -> Int -> Int
calculateProportion input x =  
    round (fromIntegral x * fromIntegral (numberOfSeats input) / fromIntegral (numberOfVoters input))
