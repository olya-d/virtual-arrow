module VirtualArrow.Factory (
    input,
    input2,
    oneDistrictProportionalityResult, 
    bordaCountResult,
    pluralityResult,
    runOffPluralityResult,
    multiDistrictProportionalityResult,
    mixedMember1Result,
    mixedMember2Result,
    thresholdProportionalityResult04,
    thresholdProportionalityResult02,
    singleTransferableVoteResult
) where

import VirtualArrow.Input
import qualified Data.Vector as V


districtsFactory :: [District]
districtsFactory =
    [ District{districtID=1, nseats=4}
    , District{districtID=2, nseats=2}
    ]

districtsFactory2 :: [District]
districtsFactory2 =
    [ District{districtID=1, nseats=2}
    , District{districtID=2, nseats=1}
    ]

votersFactory :: [Voter]
votersFactory =
    [ Voter{voterID=1, district=1, preferences= V.fromList [0,1,2]}
    , Voter{voterID=2, district=1, preferences= V.fromList [1,2,0]}
    , Voter{voterID=3, district=1, preferences= V.fromList [0,2,1]}
    , Voter{voterID=4, district=1, preferences= V.fromList [0,1,2]}
    , Voter{voterID=5, district=1, preferences= V.fromList [1,0,2]}
    , Voter{voterID=6, district=2, preferences= V.fromList [1,0,2]}
    , Voter{voterID=7, district=2, preferences= V.fromList [1,0,2]}
    , Voter{voterID=8, district=2, preferences= V.fromList [1,2,0]}
    ]

input :: Input
input = Input{districts=districtsFactory, voters=votersFactory, nparties=3}

input2 :: Input
input2 = Input{districts=districtsFactory2, voters=votersFactory, nparties=3}

oneDistrictProportionalityResult :: Parliament
oneDistrictProportionalityResult = [(0, 2), (1, 4)]

bordaCountResult :: Parliament
bordaCountResult = [(0, 4), (1, 2)]

pluralityResult :: Parliament
pluralityResult = [(0, 4), (1, 2)]

runOffPluralityResult :: Parliament
runOffPluralityResult = [(0, 4), (1, 2)]

multiDistrictProportionalityResult :: Parliament
multiDistrictProportionalityResult = [(0, 4), (1, 2)]

mixedMember1Result :: Parliament
mixedMember1Result = [(0, 4), (1, 2)]

mixedMember2Result :: Parliament
mixedMember2Result = [(0, 4), (1, 2)]

thresholdProportionalityResult04 :: Parliament
thresholdProportionalityResult04 = [(1, 6)]

thresholdProportionalityResult02 :: Parliament
thresholdProportionalityResult02 = [(1, 4), (0, 2)]

singleTransferableVoteResult :: Parliament
singleTransferableVoteResult = [(0, 3)]