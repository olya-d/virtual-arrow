module VirtualArrow.Factory (
    input, 
    oneDistrictProportionalityResult, 
    bordaCountResult,
    pluralityResult
) where

import VirtualArrow.Input


districtsFactory :: [District]
districtsFactory =
    [ District{districtID=1, seats=4}
    , District{districtID=2, seats=2}
    ]

votersFactory :: [Voter]
votersFactory =
    [ Voter{voterID=1, district=1, preferences=[0,1,2], probabilities=[0.5, 0.25, 0.25]}
    , Voter{voterID=2, district=1, preferences=[1,2,0], probabilities=[0.5, 0.25, 0.25]}
    , Voter{voterID=3, district=1, preferences=[0,2,1], probabilities=[0.5, 0.25, 0.25]}
    , Voter{voterID=4, district=1, preferences=[0,1,2], probabilities=[0.5, 0.25, 0.25]}
    , Voter{voterID=5, district=1, preferences=[1,0,2], probabilities=[0.5, 0.25, 0.25]}
    , Voter{voterID=6, district=2, preferences=[1,0,2], probabilities=[0.5, 0.25, 0.25]}
    , Voter{voterID=7, district=2, preferences=[1,0,2], probabilities=[0.5, 0.25, 0.25]}
    , Voter{voterID=8, district=2, preferences=[1,2,0], probabilities=[0.5, 0.25, 0.25]}
    ]

partiesFactory :: [Party]
partiesFactory =
    [ Party{partyID=0}
    , Party{partyID=1}
    , Party{partyID=2}
    ]

input :: Input
input = Input{districts=districtsFactory, voters=votersFactory, numOfParties=3}

oneDistrictProportionalityResult :: Parliament
oneDistrictProportionalityResult = [(0, 2), (1, 4)]

bordaCountResult :: Parliament
bordaCountResult = [(0, 4), (1, 2)]

pluralityResult :: Parliament
pluralityResult = [(0, 4), (1, 2)]