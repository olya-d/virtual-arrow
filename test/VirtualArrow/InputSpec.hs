module VirtualArrow.InputSpec (main, spec) where

import Test.Hspec
import VirtualArrow.Factory (input)
import VirtualArrow.Input

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "VirtualArrow.Input.getListOfNumberOfSeats" $
        it "returns the list of number of seats for every district" $
            getListOfNumberOfSeats input `shouldBe` [4, 2]

    describe "VirtualArrow.Input.getVotersByDistrictID" $
        it "returns list of voters in the district" $ do
            map voterID (getVotersByDistrictID input 1) `shouldBe` [1,2,3,4,5]
            map voterID (getVotersByDistrictID input 2) `shouldBe` [6,7,8]

    describe "VirtualArrow.Input.numberOfSeats" $
        it "returns the total number of seats" $
            numberOfSeats input `shouldBe` 6

    describe "VirtualArrow.Input.numberOfVoters" $
        it "returns the total number of voters" $
            numberOfVoters input `shouldBe` 8

    describe "VirtualArrow.Input.calculateProportion" $
        it "returns the number of seats based on the number of votes" $ do
            calculateProportion input 3 `shouldBe` 2
            calculateProportion input 5 `shouldBe` 4
