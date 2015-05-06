module VirtualArrow.InputSpec (main, spec) where

import Test.Hspec
import VirtualArrow.Factory (input)
import VirtualArrow.Input

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "VirtualArrow.Input.listOfNumberOfSeats" $
        it "returns the list of number of seats for every district" $
            listOfNumberOfSeats input `shouldBe` [4, 2]

    describe "VirtualArrow.Input.votersByDistrictID" $
        it "returns list of voters in the district" $ do
            map voterID (votersByDistrictID input 1) `shouldBe` [1,2,3,4,5]
            map voterID (votersByDistrictID input 2) `shouldBe` [6,7,8]

    describe "VirtualArrow.Input.votersByDistrict" $
        it "returns list of voters in the each district" $
            map (\x -> (fst x, map voterID (snd x))) (votersByDistrict input) `shouldBe` [(1, [1,2,3,4,5]), (2, [6,7,8])]

    describe "VirtualArrow.Input.numberOfSeatsByDistrictID" $
        it "returns list of number of seats in the district" $ do
            numberOfSeatsByDistrictID input 1 `shouldBe` 4
            numberOfSeatsByDistrictID input 2 `shouldBe` 2

    describe "VirtualArrow.Input.numberOfSeatsByDistrict" $
        it "returns the total number of seats" $
            numberOfSeatsByDistrict input `shouldBe` [4, 2]

    describe "VirtualArrow.Input.firstChoices" $
        it "returns list of the first choice for each voter" $
            firstChoices input `shouldBe` [0, 1, 0, 0, 1, 1, 1, 1]

    describe "VirtualArrow.Input.firstChoicesAmongVoters" $
        it "returns list of the first choice for each voter" $ do
            firstChoicesAmongVoters (votersByDistrictID input 1) `shouldBe` [0, 1, 0, 0, 1]
            firstChoicesAmongVoters (votersByDistrictID input 2) `shouldBe` [1, 1, 1]

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
