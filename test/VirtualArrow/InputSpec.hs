module VirtualArrow.InputSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

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
    [ Party{partyID=1}
    , Party{partyID=2}
    , Party{partyID=3}
    ]

input = Input{districts=districtsFactory, voters=votersFactory}


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "VirtaulArrow.Input.getListOfNumberOfSeats" $
        it "returns the list of number of seats for every district" $
            getListOfNumberOfSeats input `shouldBe` [4, 2]

    describe "VirtaulArrow.Input.getVotersByDistrictID" $
        it "returns list of voters in the district" $ do
            map voterID (getVotersByDistrictID input 1) `shouldBe` [1,2,3,4,5]
            map voterID (getVotersByDistrictID input 2) `shouldBe` [6,7,8]

    describe "VirtaulArrow.Input.numberOfSeats" $
        it "returns the total number of seats" $
            numberOfSeats input `shouldBe` 6

    describe "VirtaulArrow.Input.numberOfVoters" $
        it "returns the total number of voters" $
            numberOfVoters input `shouldBe` 8

    describe "VirtaulArrow.Input.calculateProportion" $
        it "returns the number of seats based on the number of votes" $ do
            calculateProportion input 3 `shouldBe` 2
            calculateProportion input 5 `shouldBe` 4
