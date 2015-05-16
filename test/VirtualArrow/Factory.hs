module VirtualArrow.Factory (
    input,
    input2,
    oneDistrictProportionalityResult, 
    bordaCountResult,
    bordaCountR,
    pluralityResult,
    runOffPluralityResult,
    multiDistrictProportionalityResult,
    mixedMember1Result1,
    mixedMember1Result0,
    mixedMember1Result05,
    mixedMember2Result,
    mixedMember2R,
    thresholdProportionalityResult05,
    thresholdProportionalityResult02,
    singleTransferableVoteResult
) where

import VirtualArrow.Input
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map


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
    [ Voter{district=1, preferences=V.fromList [0,1,2]}
    , Voter{district=1, preferences=V.fromList [1,2,0]}
    , Voter{district=1, preferences=V.fromList [0,2,1]}
    , Voter{district=1, preferences=V.fromList [0,1,2]}
    , Voter{district=1, preferences=V.fromList [1,0,2]}
    , Voter{district=2, preferences=V.fromList [1,0,2]}
    , Voter{district=2, preferences=V.fromList [1,0,2]}
    , Voter{district=2, preferences=V.fromList [1,2,0]}
    ]

input :: Input
input = Input{ districts=districtsFactory
             , voters=votersFactory
             , nparties=3
             , districtMap=Map.fromList $ votersByDistrict votersFactory}

input2 :: Input
input2 = Input{ districts=districtsFactory2
              , voters=votersFactory
              , nparties=3
              , districtMap=Map.fromList $ votersByDistrict votersFactory}

oneDistrictProportionalityResult :: Parliament
oneDistrictProportionalityResult = [(0, 2), (1, 4)]

bordaCountResult :: Parliament
bordaCountResult = [(0, 4), (1, 2)]
bordaCountR :: Double
bordaCountR = sqrt(1/2 * (4 + 4))/6*100

pluralityResult :: Parliament
pluralityResult = [(0, 4), (1, 2)]

runOffPluralityResult :: Parliament
runOffPluralityResult = [(0, 4), (1, 2)]

multiDistrictProportionalityResult :: Parliament
multiDistrictProportionalityResult = [(0, 2), (1, 4)]

mixedMember1Result1 :: Parliament
mixedMember1Result1 = [(0, 4), (1, 2)]

mixedMember1Result0 :: Parliament
mixedMember1Result0 = [(0, 2), (1, 4)]

mixedMember1Result05 :: Parliament
mixedMember1Result05 = [(0, 3), (1, 3)]

mixedMember2Result :: Parliament
mixedMember2Result = [(0, 3), (1, 3)]
mixedMember2R :: Double
mixedMember2R = sqrt(1/2 * (1 + 1))/6*100

thresholdProportionalityResult05 :: Parliament
thresholdProportionalityResult05 = [(1, 6)]

thresholdProportionalityResult02 :: Parliament
thresholdProportionalityResult02 = [(0, 2), (1, 4)]

singleTransferableVoteResult :: Parliament
singleTransferableVoteResult = [(0, 3)]

